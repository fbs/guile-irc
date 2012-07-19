;; Copyright (C) 2012 bas smit (fbs)
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

(define-module (irc irc)
  #:version (0 2)
  #:use-module ((irc message)
		#:renamer (symbol-prefix-proc 'msg:))
  #:use-module (irc tagged-hook)
  #:use-module (irc channel)
  #:use-module (irc error)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:export (add-message-hook!
	    add-simple-message-hook!
	    channels->list
	    connected?
	    do-close
	    do-command
	    do-connect
	    do-join
	    do-listen
	    do-nick
	    do-part
	    do-privmsg
	    do-quit
	    do-register
	    do-runloop
	    do-wait
	    exists-message-hook?
	    hostname
	    in-channel?
	    irc-object?
	    make-irc
	    nick
	    password
	    port
	    realname
	    remove-message-hook!
	    reset-message-hook!
	    run-message-hook
	    server
	    set-hostname!
	    set-password!
	    set-port!
	    set-realname!
	    set-server!))

;; (use-modules ((irc message)
;; 	      #:renamer (symbol-prefix-proc 'msg:))
;; 	     (irc tagged-hook)
;; 	     (irc channel)
;; 	     (irc error)
;; 	     (ice-9 format)
;; 	     (ice-9 rdelim))

;;;; Some constants
(define *nick* "bot")
(define *realname* "mr bot")
(define *server* "localhost")
(define *port* 6667)
(define *hostname* "localhost")
(define *quitmsg* "Not enough parenthesis")

(define *max-msg-length* 512)

;;;; macros

;;;; Data types
;; Channels:   Hashmap          containing all the joined channels.
;; Connected:   Boolean          #t if connected to a server #f otherwise.
;; Hooks:       Tagged-hook      hooks to run on messages.
;; Hostname:    String           irc hostname
;; Nick:        String           irc nickname
;; password:    String #f        irc password
;; port:        Number           irc port
;; realname:    String           irc realname
;; server:      String           irc server url
;; socket:      Socket           Socket

(define irc-object
  (make-record-type
   "irc"
   '(channels
     connected
     hooks
     hostname
     nick
     port
     realname
     server
     socket)
   (lambda (obj port)
     (format port "#<~A~c irc object>"
	     ((record-accessor irc-object 'server) obj)
	     (if ((record-accessor irc-object 'connected) obj)
		 #\!
		 #\?)))))

;;;; Internal procedures
(define channels	 (record-accessor irc-object 'channels))
(define hooks		 (record-accessor irc-object 'hooks))
(define _socket		 (record-accessor irc-object 'socket))

(define (symbolize c)
"Symbolize returns a symbol if @var{c} is a symbol or string, #f otherwise."
  (cond
   ((symbol? c) c)
   ((string? c) (string->symbol c))
   (else #f)))

(define (stringify c)
  "Stringify returns a string if @var{c} is a symbol or string, #f otherwise."
  (cond
   ((string? c) c)
   ((symbol? c) (symbol->string c))
   (else #f)))

(define (send-string obj fmt . args)
  "Send (format) string @var{fmt} to the server. The crlf is handled by this
procedure, so don't add them! Uses (ice-9 format) for the formatting."
  (send-raw obj (apply format #f fmt args)))

(define (send-message obj msg)
  "Send irc-message @var{msg} to the server."
  (define split-long-message list)
  (for-each (lambda (m) (send-raw obj (msg:message->string m)))
	    (split-long-message msg)))

(define (send-raw obj str)
  "Send string @var{str} to the server"
  (let ([msg (string-append str "\r\n")])
    (send (_socket obj) msg)))

(define (read-message obj)
  "Read a parsed message from irc-object obj."
  (define (delete-return s)
    (string-delete s (string->char-set "\r")))
  (let* ([s (_socket obj)]
	 [m (and (char-ready? s) (read-line s))])
    (cond
     ((eof-object? m) #f)
     (m (msg:parse-message-string (delete-return m)))
     (else #f))))

(define* (cleanup-irc-object obj)
  "Reset @var{channels}, @var{connected} and socket to their initial value.
The default behaviour is to also reset the message-hook. Set handlers
to #f to disable."
  (channel-clear! (channels obj))
  ((record-modifier irc-object 'connected) obj #f)
  (and (_socket obj) (close-port (_socket obj)))
  ((record-modifier irc-object 'socket) obj #f))

(define (irc-object? obj)
  ((record-predicate irc-object) obj))

(define nick		 (record-accessor irc-object 'nick))
(define port		 (record-accessor irc-object 'port))
(define realname	 (record-accessor irc-object 'realname))
(define server		 (record-accessor irc-object 'server))
(define hostname	 (record-accessor irc-object 'hostname))
(define connected?	 (record-accessor irc-object 'connected))

(define* (make-irc #:key (nick *nick*) (realname *nick*) (server *server*)
		   (port *port*) (hostname *hostname*))
  "Create a new irc object.
nick: string
realname: string
server: string
port: integer
hostname: string."
  ((record-constructor irc-object)
   (make-channel-table)	;; channels
   #f			;; connected
   (make-tagged-hook)	;; hooks
   hostname		;; hostname
   nick			;; nick
   port			;; port
   realname		;; realname
   server		;; server
   #f			;; socket
   ))

(define (channels->list obj)
  "Return the channels joined by irc-object @var{obj} as list."
  (channel-table->list (channels obj)))

(define (in-channel? obj chan)
  "Check if channel @var{chan} in irc-object @var{obj} is joined."
  (cond
   ((string? chan) (channel-ref (channels obj) chan))
   ((symbol? chan) (channel-ref (channels obj) (symbol->string chan)))
   (else (irc-type-error "in-channel?" 'chan "string or symbol" chan))))

(define (set-port! obj var)
  "If not yet connected change port to @var{var}."
  (if (connected? obj)
      (irc-error "set-port!: impossible to change port when connected.")
      (if (< 0 var 65536)
	  ((record-modifier irc-object 'port) obj var)
	  (irc-error "set-port!: invalid port number."))))

(define (set-server! obj var)
  "If not yet connected change server to @var{var}."
  (if (connected? obj)
      (irc-error "set-server!: impossible to change server when connected.")
      (if (= 0 (string-length var))
	  ((record-modifier irc-object 'server) obj *server*)
	  ((record-modifier irc-object 'server) obj var))))

(define (set-realname! obj var)
  "If not yet connected change realname to @var{var}."
  (if (connected? obj)
      (irc-error "set-realname!: impossible to change realname when connected.")
      (if (= 0 (string-length var))
	  ((record-modifier irc-object 'realname) obj *realname*)
	  ((record-modifier irc-object 'realname) obj var))))

(define (set-hostname! obj var)
  "If not yet connected change hostname to @var{var}."
  (if (connected? obj)
      (irc-error "set-hostname!: impossible to change hostname when connected.")
      (if (= 0 (string-length var))
	  ((record-modifier irc-object 'hostname) obj *hostname*)
	  ((record-modifier irc-object 'hostname) obj var))))

(define (set-nick! obj var)
  "If not yet connected change nick to @var{var}."
  (if (connected? obj)
      (irc-error "set-nick!: impossible to change nick when connected.")
      (if (= 0 (string-length var))
	  ((record-modifier irc-object 'nick) obj *nick*)
	  ((record-modifier irc-object 'nick) obj var))))

(define (do-nick obj nick)
  "Try to change the nickname into @var{nick}. When the nick is already taken keep the old nick.
returns #f, else #t."
  (if (not (string? nick))
      (irc-type-error "set-nick!" "string" nick)
      (if (not (connected? obj))
	  ((record-modifier irc-object 'nick) obj nick)
	  (do-command obj #:command 'NICK #:middle nick))))

(define (do-connect obj)
  "Try to connect object to the specified server."
  (define (_connect ircobj)
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
      (irc-error "do-connect: expected obj to be an irc-object but got ~a." obj)
      (catch #t
	(lambda ()
	  (_connect obj))
	(lambda (key . args)
	  (irc-error "do-connect: failed to connect to server ~a." (server obj))))))

(define (do-register obj)
  "Send nick and user commands."
  (define (check-nick-valid obj)
    (let ([msg (do-listen obj)])
      (cond [(eq? msg #f) #t]
	    [(and (msg:message? msg)
		  (eq? (msg:command msg) 433)) #f]
	    [else (check-nick-valid obj)])))
  (define (try-nick nick)
    (do-command obj #:command 'NICK #:middle nick)
    (sleep 2)
    (if (check-nick-valid obj)
	nick
	(try-nick (string-append nick "_"))))
  (define (try-user)
    (send-string obj "USER ~a ~a * :~a" (nick obj) (hostname obj) (realname obj)))

  (if (not (connected? obj))
      (do-connect obj))
  ((record-modifier irc-object 'nick) obj (try-nick (nick obj)))
  (try-user))

(define* (do-close obj)
  "Close the connection without sending QUIT."
  (cleanup-irc-object obj))

(define* (do-quit obj #:key (quit-msg *quitmsg*))
  "Send QUIT to the server and clean up."
  (if (connected? obj)
      (begin (send-string obj "QUIT :~a" quit-msg)
	     (do-close obj))
      (do-close obj)))

(define (do-privmsg obj receiver msg)
  "Send message @var{msg} to @var{reciever} (channel or user)."
  (send-string obj "PRIVMSG ~a :~a" receiver msg))

(define* (do-command obj #:key command middle trailing)
  (send-message obj
   (msg:make-message #:command command #:middle middle #:trailing trailing)))

(define (do-listen obj)
  "Return a parsed message (see the message module) if there is data available,
#f otherwise."
  (if (not (connected? obj))
      #f
      (read-message obj)))

(define (do-wait obj)
  "Wait till data is available."
  (let loop ([m (do-listen obj)])
    (if m
	m
	(loop (do-listen obj)))))

(define (do-join obj chan . pass)
  "Try to join channel @var{chan}."
  (if (not (msg:is-channel? chan))
      (irc-error "invalid channel" chan))
  (if (null? pass)
      (send-string obj "JOIN ~a" chan)
      (send-string obj "JOIN ~a ~a" chan pass))
  (channel-add! (channels obj) chan))

(define (do-runloop obj)
  (let ([sock (_socket obj)])
    (while (not (port-closed? sock))
      (handle-message obj (do-wait obj)))))

(define (do-part obj chan)
  "Part channel @var{chan}."
  (if (in-channel? obj chan)
      (send-string obj "PART ~a" chan)
      #f))

(define* (add-message-hook!
	  obj proc #:key tag append)
  "Install procedure @var{proc} as new message-hook. proc will be called as
 (proc irc-message).

If a tag is given it will be used to identify the procedure. If not tag is set
 or when the tag is #f no tag will be used. Note that it is impossible to
 remove procedures without tags from the hook (unless you reset the hook).

Procedures will be added to the front of the hook unless append is not #f."
    (add-tagged-hook! (hooks obj) proc tag append))

(define* (add-simple-message-hook!
	 obj proc #:key sender receiver command middle trailing tag append)
  (let ([handler
	(lambda (msg)
	  (let ([cmd (msg:command msg)]
		[send (msg:parse-source msg)]
		[rec (msg:middle msg)]
		[trail (msg:trailing msg)])
	    (and
	     (or (not command)
		 (if (procedure? command)
		     (command cmd)
		     (eq? command cmd)))
	     (or (not sender)
		 (if (procedure? sender)
		     (sender send)
		     (string=? send sender)))
	     (or (not receiver)
		 (if (procedure? receiver)
		     (receiver rec)
		     (string=? rec receiver)))
	     (or (not trailing)
		 (if (procedure? trailing)
		     (trailing trail)
		     (string-contains trail trailing)))
	     (proc msg))))])
    (add-tagged-hook! (hooks obj) handler tag append)))

(define (exists-message-hook? obj tag)
  "Returns #t if a hook with tag @var{tag} exists, #f otherwise."
  (->bool (find-tagged-hook (hooks obj) tag)))

(define (remove-message-hook! obj tag)
  "Remove all procedures in the hook that match tag @var{tag}."
  (remove-tagged-hook! (hooks obj) tag))

(define (run-message-hook obj msg)
  "Apply arguments @var{msg} to all the procedures in the hook."
  (run-tagged-hook (hooks obj) msg))

(define (reset-message-hook! obj)
  "Remove all the message-hooks from irc-object @var{obj}."
  (reset-tagged-hook! (hooks obj)))

(define handle-message run-message-hook)
