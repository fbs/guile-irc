;; Copyright (C) 2012, 2013 bas smit (fbs)
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

(define-module (irc network)
  #:version (0 3 0)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 rdelim)
  #:use-module ((irc gnutls) #:renamer (symbol-prefix-proc 'tls:))
  #:use-module (irc error)
  #:export (
            create
            (do-connect . connect)
            send
            receive
            data-ready?
            cleanup
            (_close . close)
            get-socket
            connected?
            ssl?
            ))

;; Errors

(define-error error-eof     'irc:network:eof)
(define-error error-connect 'irc:network:connect)

;; Types

(define-record-type <network>
  (make-network
   address
   port
   socket
   addrinfo
   ssl
   loghook
   connected?)
  network?
  (address get-address set-address!)
  (port get-port set-port!)
  (socket get-socket set-socket!)
  (addrinfo get-addrinfo set-addrinfo!)
  (ssl get-ssl set-ssl!)
  (loghook get-loghook set-loghook!)
  (connected? connected? set-connected?!))

(set-record-type-printer!
 <network>
 (lambda (record port)
   (write-char #\< port)
   (display "network: ")
   (display (get-address record) port)
   (if (get-ssl record) (display "/ssl") port)
   (if (connected? record)
       (display "!>" port)
       (display "?>" port))))

(define (ssl? obj)
  (->bool (get-ssl obj)))

;; Private

(define (enable-gnutls-debug)
  (tls:enable-global-logging! 10 #f))

(define (disable-gnutls-debug)
  (tls:disable-global-logging!))

(define (get-ai-sock address family)
  (let ([ai (car (getaddrinfo address "irc" family))])
    (values ai (socket (addrinfo:fam ai)
                         (addrinfo:socktype ai)
                         (addrinfo:protocol ai)))))

;; Public

(define* (create #:key (address "localhost") (port 6697) (family PF_INET) (ssl #f))
  "Create a new network.
address: Address to connect.
port: port to connect to.
family: Socket family (see manual  7.2.11)
tls: If set to #t use ssl (requires gnutls)"
  (let-values ([(ai sock) (get-ai-sock address family)])
    (make-network
     address ;; address
     port    ;; port
     sock    ;; socket
     ai      ;; addrinfo
     (if ssl ;; ssl
         (tls:wrap-ssl sock)
         #f)
     #f      ;; loghook
     #f      ;; connected
     )))


(define (do-connect obj)
  "connect to server"
  (if (not (connected? obj))
      (let ([ai (get-addrinfo obj)])
        (catch #t
          (lambda ()
            (connect (get-socket obj)
                     (addrinfo:fam ai)
                     (sockaddr:addr (addrinfo:addr ai))
                     (get-port obj)))
          (lambda (key . params)
            (error-connect "Unable to connect.")))
        (if (get-ssl obj)
            (tls:handshake (get-ssl obj)))
        (set-connected?! obj #t))
      #t))

(define (_close obj)
  (if (get-ssl obj)
      (tls:close/cleanup (get-ssl obj)))
  (close (get-socket obj))
  (set-socket! obj #f)
  (set-ssl! obj #f))

(define (cleanup obj)
  (if (connected? obj)
      (close obj))
  (set-address! obj #f)
  (set-port! obj #f)
  (set-addrinfo! obj #f)
  (set-loghook! obj #f)
  (set-connected?! obj #f))

(define (send obj msg)
"Try sending data."
  (if (connected? obj)
      (if (ssl? obj)
          (tls:send (get-ssl obj) msg)
          (display msg (get-socket obj)))
      (error-connect "Not connected")))

(define (receive obj)
"Try to read a line (using read-line)."
  (if (connected? obj)
      (let ([msg (if (ssl? obj)
                     (tls:receive (get-ssl obj))
                     (read-line (get-socket obj)))])
        ;; If an eof-object is read the port is closed.
        (if (eof-object? msg)
            (begin
              (_close obj)
              (error-eof "Unexpected EOF, closing session."))
            msg))
      (error-connect "Not connected")))

(define (data-ready? obj)
"Check if data is available for reading."
  (if (connected? obj)
      (if (ssl? obj)
          (tls:data-ready? (get-ssl obj))
          (char-ready? (get-socket obj)))
      (error-connect "Not connected")))
