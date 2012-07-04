;; Copyright (C) 2012 bas smit (fbs)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

(define-module (irc irc)
  #:version (0 0)
  #:use-module ((irc message)
		#:renamer (symbol-prefix-proc 'irc:))
  #:use-module (irc tagged-hook)
  #:use-module (irc error)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:export (nick
	    password
	    realname
	    server
	    connected?
	    channels-list
	    in-channel?

	    make-irc-object

	    set-server!
	    set-realname!
	    set-password!
	    set-hostname!
	    do-nick
	    do-connect
	    do-register
	    do-quit
	    do-part
	    do-join
	    do-privmsg
	    do-raw
	    do-topic))

;;;; Some constants
(define *nick* "bot")
(define *server* "localhost")
(define *port* 6667)
(define *hostname* "localhost")
(define *quitmsg* "Not enough parenthesis")

;;;; macros


;;;; Data types

;; Channels are stored in a hashmap <symbol channel name, #t>
;; Raw-hooks are ran on each incomming message before they're parsed.
;; log-handler same
;; hooks are user specified hooks that are matched against parts of the parsed message.
(define irc-object
  (make-record-type
   "irc"
   '(channels
     connected
     hostname
     nick
     password
     port
     realname
     server
     socket

     hooks
     log-handler
     raw-hooks)
   (lambda (obj port)
     (format port "#<~A irc object>"
	     ((record-accessor irc-object 'server) obj)))))

;;;; Internal procedures
(define channels	 (record-accessor irc-object 'channels))
(define raw-hooks	 (record-accessor irc-object 'raw-hooks))
(define hooks		 (record-accessor irc-object 'hooks))
(define _socket		 (record-accessor irc-object 'socket))

(define (symbolize c)
  (cond
   ((symbol? c) c)
   ((string? c) (string->symbol c))
   (else #f)))

;;; Channel table
(define (make-channel-table)
  (make-hash-table 31))

(define (channel-add table channel)
  (let ([chan (symbolize channel)])
    (if (not chan)
	(irc-type-error "channel-add" "string or symbol" channel)
	(hash-set! table chan #t))))

(define (channel-remove table channel)
  (let ([chan (symbolize channel)])
    (if (not chan)
	(irc-type-error "channel-remove" "string or symbol" channel)
	(hash-remove! table chan))))

(define channel-clear! hash-clear!)

(define (channel-table->list table)
  (hash-map->list (lambda (x y) x) table))

(define (channel-ref table channel)
  (let ([chan (symbolize channel)])
    (if (not chan)
	(irc-type-error "channel-ref" "string or symbol" channel)
	(hash-ref table chan))))

(define (cleanup-irc-object obj)
  "Reset the object."
  (channel-clear! (channels obj))
  ((record-modifier irc-object 'connected) obj #f)
  (close-port (_socket obj))
  ((record-modifier irc-object 'socket) obj #f)
  (reset-hook! (raw-hooks obj))
  (reset-hook! (hooks obj))
  ((record-modifier irc-object 'log-handler) #f))

(define (send-message obj msg . args)
  "Format `args' into `msg' and append newline to it before sending it to the server."
  (let ([socket (_socket obj)])
    (apply format `(,socket ,msg ,@args))
    (send socket "\r\n")
    ;; todo check errors
))

(define (send-raw obj msg)
  "Send the message 'as-is'."
  (send (_socket obj) msg))

(define (try-nick obj nick)
  (send-message obj "NICK ~a" nick)
  ;; TODO: parse results and check if nick is accepted
  )

;;; Public functions
(define irc-object?	 (record-predicate irc-object))
(define nick		 (record-accessor irc-object 'nick))
(define password	 (record-accessor irc-object 'password))
(define port		 (record-accessor irc-object 'port))
(define realname	 (record-accessor irc-object 'realname))
(define server		 (record-accessor irc-object 'server))
(define hostname	 (record-accessor irc-object 'hostname))
(define connected?	 (record-accessor irc-object 'connected))

(define* (make-irc-object #:key (nick *nick*) (realname *nick*) (server *server*)
			  (port *port*) (password '()) (hostname *hostname*))
  ((record-constructor irc-object)
   (make-channel-table)	;; channels
   #f			;; connected
   hostname		;; hostname
   nick			;; nick
   password		;; password
   port			;; port
   realname		;; realname
   server		;; server
   #f			;; socket
   (make-hook 4)	;; hooks
   #f			;; log-handler
   (make-hook 1)	;; raw-hook
   ))

(define (channels->list obj)
  "Return the channels joined by irc-object `obj' as list."
  (channel-table->list (channels obj)))

(define (in-channel? obj chan)
  "Check if channel `chan' in irc-object `obj' is joined."
  (channel-ref (channels obj) chan))

(define (set-server! obj srv)
  "If not yet connected change server to `srv'."
  (if (not (connected? obj))
      (irc-error "set-server!: impossible to change server when connected.\n")
      (if (not (string? srv))
	  (irc-type-error "set-server!" "string" srv)
	  ((record-modifier irc-object 'server) obj srv))))

(define (set-realname! obj rn)
  "If not yet connected change realname to `rn'."
  (if (not (connected? obj))
      (irc-error "set-realname!: impossible to change realname when connected.\n")
      (if (not (string? rn))
	  (irc-type-error "set-realname!" "string" rn)
	  ((record-modifier irc-object 'realname) obj rn))))

(define (set-password! obj pwd)
  "If not yet connected change password to `pwd'."
  (if (not (connected? obj))
      (irc-error "set-password!: impossible to change password when connected.\n")
      (if (not (string? pwd))
	  (irc-type-error "set-password!" "string" pwd)
	  ((record-modifier irc-object 'password) obj pwd))))

(define (set-hostname! obj hn)
  "If not yet connected change hostname to `hn'."
  (if (not (connected? obj))
      (irc-error "set-hostname!: impossible to change hostname when connected.\n")
      (if (not (string? hn))
	  (irc-type-error "set-hostname!" "string" hn)
	  ((record-modifier irc-object 'hostname) obj hn))))

(define (do-nick obj nick)
  "Try to change the nickname into `nick'. When the nick is already taken keep the old nick.
returns #f, else #t."
  (if (not (string? nick))
      (irc-type-error "set-nick!" "string" nick)
      (if (not (connected? obj))
	  ((record-modifier irc-object 'nick) obj nick)
	  (if (try-nick obj nick)
	      ((record-modifier irc-object 'nick) obj nick)
	      #f))))

(define (do-connect obj)
  "Try to connect object to the specified server."
  (define (_connect ircobj)
    "Setup a connection with the server and"
    (let* ([ai (car (getaddrinfo (server ircobj) "ircd"))]
	   [s  (socket (addrinfo:fam ai)
		       (addrinfo:socktype ai)
		       (addrinfo:protocol ai))])
      (connect s
	       (addrinfo:fam ai)
	       (sockaddr:addr (addrinfo:addr ai))
	       (port ircobj))
      ((record-modifier irc-object 'socket) ircobj s)
      ((record-modifier irc-object 'connected) ircobj #t)))

  (if (not (irc-object? obj))
      (irc-error "do-connect: expected obj to be an irc-object but got ~a.\n" obj)
      (catch #t
	(lambda ()
	  (_connect obj))
	(lambda (key . args)
	  (irc-error "do-connect: failed to connect to server ~a.\n" (server obj))))))

(define (do-register obj)
  ;; FIXME
  "Send password, nick and user commands."
  (define (send-pass)
    (if (not (null? (password obj)))
	(send-message obj "PASS ~a" (password obj))))
  (define (send-nick)
    (send-message obj "NICK ~a" (nick obj)))
  (define (send-user)
    (send-message obj "USER ~a ~a * :~a" (nick obj) (hostname obj) (realname obj)))
  (if (not (connected? obj))
      #f)
  (send-pass)
  (send-nick)
  (send-user)
  #t)

(define (do-quit obj . msg)
  "Send QUIT to the server and clean up."
  (if (null? msg)
      (send-message obj "QUIT :~a" *quitmsg*)
      (send-message obj "QUIT :~a" msg))
  (cleanup-irc-object obj))

(define (do-privmsg obj receiver msg)
  "Send message `msg' to `reciever' (channel or user)."
  (send-message obj "PRIVMSG ~a :~a" receiver msg)
  ;; TODO: check success
  )

(define (do-raw obj msg)
  "Send unformatted message `msg' to the server."
  (send-raw obj msg))

(define (do-topic obj chan . msg) #f)
