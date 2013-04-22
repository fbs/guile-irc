;; Copyright (C) 2013 bas smit (fbs)
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; A small wrapper around guile-gnutls

(define-module (irc gnutls)
  #:version (0 3 0)
  #:use-module ((gnutls) #:renamer (symbol-prefix-proc 'tls:))
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-2)
  #:use-module (ice-9 rdelim)
  #:export (wrap-ssl
            connect!
            close/cleanup
            data-ready?
            receive
            send
            handshake
            rehandshake
            disable-global-logging!
            enable-global-logging!
            set-dh-prime-bits!
            get-session
            get-credentials
            get-compression-mode
            get-cipher
            get-kx
            get-protocol
            get-mac
            ))

(define *priority* "NORMAL")
(define *dh-bits* 1024)

;; Syntax Magic

(define-syntax ssl-error
  (syntax-rules ()
    ((_ msg)
     (throw 'ssl-error msg))
    ((_ msg ...)
     (throw 'ssl-error (format #f msg ...)))))

(define-syntax ssl-type-error
  (syntax-rules ()
    ((_ func expect)
     (throw 'ssl-error (format #f "~a: expected type ~a" func expect)))
    ((_ func expect got)
     (throw 'ssl-error (format #f "~a: expected type ~a, got ~a" func expect got)))))

;; Custom types

(define-record-type <ssl>
  (make-ssl 
   session
   credentials
   auth-method ;; x509, anon; not yet implemented
   socket
   connected?
   handshake?)
  ssl? 
  (session get-session set-session!)
  (credentials get-credentials set-credentials!)
  (priority get-priority set-priority!)
  (auth-method get-auth-method set-auth-method!)
  (socket get-socket set-socket!)
  (addrinfo get-addrinfo set-addrinfo!)
  (connected? get-connected? set-connected?!)
  (handshake? get-handshake? set-handshake?!))

(set-record-type-printer! 
 <ssl>
 (lambda (record port)
   (display "#<ssl>" port)))

;; Private

(define (create/connect addr port)
  (let* ([ai (car (getaddrinfo addr "ircd"))]
         [sock (socket (addrinfo:fam ai)
                       (addrinfo:socktype ai)
                       (addrinfo:protocol ai))]
         [ssl (wrap-ssl sock)])
    (connect sock
             (addrinfo:fam ai)
             (sockaddr:addr (addrinfo:addr ai))
             port)
    (handshake ssl)
    ssl))

;; Public 

(define (wrap-ssl socket)
  "Take a socket and return a s."
  (let ([session (tls:make-session tls:connection-end/client)]
        [cred (tls:make-certificate-credentials)])
    (tls:set-session-default-priority! session)
    (tls:set-session-credentials! session cred)
    (tls:set-session-dh-prime-bits! session *dh-bits*)
    (tls:set-session-transport-fd! session (fileno socket))
    (make-ssl
     session  ;; session
     cred     ;; credentials
     #f       ;; auth-method
     socket   ;; socket
     #f       ;; connected?
     #f       ;; handshake?
)))

(define (close/cleanup obj)
  (catch #t
    (lambda ()
      (tls:bye (get-session obj) tls:close-request/rdwr)
      (close (get-socket obj)))
    (lambda () #t))
  (set-socket! obj #f)
  (set-session! obj #f)
  (set-credentials! obj #f)
  (set-handshake?! obj #f))

(define (data-ready? obj)
"Test if data is ready."
  (char-ready? (get-socket obj)))

(define (receive obj)
"Receive a string."
  (read-line (tls:session-record-port (get-session obj))))

(define (send obj msg)
"Send a string."
  (display msg (tls:session-record-port (get-session obj))))

(define (handshake obj)
  (if (not (get-handshake? obj))
      (begin
        (tls:handshake (get-session obj))
        (set-handshake?! obj #t))))

(define (rehanshake obj)
  (if (get-handshake? obj)
      (tls:rehandshake (get-session obj))))

(define (disable-global-logging!)
  "Disable logging (log level 0)."
  (tls:set-log-level! 0))

(define (enable-global-logging! level . proc)
  "Enable logging.
level: Desired logging level, 0-10
proc: Procedure to use for loggin, #f for logging to std-error."
  (tls:set-log-level! level)
  (if (procedure? proc)
      (tls:set-log-procedure! proc)
      (tls:set-log-procedure! (lambda (level str)
                            (format (current-error-port) "tls-socket/gnutls: ~a| ~a ~a" (getpid) level str)))))

(define (get-compression-mode obj)
  (tls:session-compression-method (get-session obj)))

(define (get-mac obj)
  (tls:session-mac (get-session obj)))

(define (get-kx obj)
  (tls:session-kx (get-session obj)))

(define (get-cipher obj)
  (tls:session-cipher (get-session obj)))

(define (get-protocol obj)
  (tls:session-protocol (get-session obj)))

