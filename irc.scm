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
  #:version (0 0)
  #:use-module ((irc message)
		#:renamer (symbol-prefix-proc 'msg:))
  #:use-module (irc tagged-hook)
  #:use-module (irc channel)
  #:use-module (irc error)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:export (connect 
	    add-message-hook!
	    add-simple-message-hook!
	    channels->list 
	    connected?       
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
	    install-hello-handler
	    install-ping-handler
	    install-printer 
	    irc-object?      
	    make-irc-object 
	    nick             
	    password         
	    port             
	    realname         
	    remove-message-hook!
	    remove-printer 
	    reset-filter!
	    reset-message-hook!
	    run-message-hook 
	    send-nick
	    send-pass
	    send-user
	    server           
	    set-filter!
	    set-hostname!
	    set-password!
	    set-port! 
	    set-realname!
	    set-server!))

(use-modules ((irc message)
	     #:renamer (symbol-prefix-proc 'msg:))
	     (irc tagged-hook)
	     (irc channel)
	     (irc error)
	     (ice-9 format)
	     (ice-9 rdelim))

;;;; Some constants
(define *nick* "bot")
(define *server* "localhost")
(define *port* 6667)
(define *hostname* "localhost")
(define *quitmsg* "Not enough parenthesis")

;;;; macros

;;;; Data types

;; Channels:  Hashmap          containing all the joined channels.
;; Connected: Boolean          #t if connected to a server #f otherwise.
;; Filter:    Procedure        filter out messages that can be ignored.
;; Hooks:     Tagged-hook      hooks to run on messages.
;; Nick:      String Symbol    irc nickname
;; Hostname:  String Symbol    irc hostname
;; password:  String Symbol #f irc password
;; port:      number           irc port
;; realname:  String Symbol    irc realname
;; server:    String           irc server url
;; socket:    Socket           Socket
(define irc-object
  (make-record-type
   "irc"
   '(channels
     connected
     filter
     hooks
     hostname
     nick
     password
     port
     realname
     server
     socket
     )
   (lambda (obj port)
     (format port "#<~A~c irc object>"
	     ((record-accessor irc-object 'server) obj)
	     (if ((record-accessor irc-object 'connected) obj)
		 #\!
		 #\?)))))

;;;; Internal procedures
(define channels	 (record-accessor irc-object 'channels))
(define _filter		 (record-accessor irc-object 'filter))
(define hooks		 (record-accessor irc-object 'hooks))
(define _socket		 (record-accessor irc-object 'socket))

(define (symbolize c)
  (cond
   ((symbol? c) c)
   ((string? c) (string->symbol c))
   (else #f)))


(define (cleanup-irc-object obj)
  "Reset the object."
  (channel-clear! (channels obj))
  ((record-modifier irc-object 'connected) obj #f)
  (close-port (_socket obj))
  ((record-modifier irc-object 'socket) obj #f)
  (reset-message-hook! obj)
  ((record-modifier irc-object 'filter) obj identity))

(define (send-message obj msg . args)
  (send-raw obj (apply format #f msg args) #t))

(define* (send-raw obj msg #:optional crlf)
  "Send message string `msg' to `obj'. If crlf is #t append \r\n to the string."
  (if (connected? obj)
      (if crlf
	  (send (_socket obj) (string-append msg "\r\n"))
	  (send (_socket obj) msg))
      (irc-error "send-raw: irc-object ~a is not connected to a server." obj)))



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
   #f			;; filter
   (make-tagged-hook)	;; hooks
   hostname		;; hostname
   nick			;; nick
   password		;; password
   port			;; port
   realname		;; realname
   server		;; server
   #f			;; socket
   ))

(define (channels->list obj)
  "Return the channels joined by irc-object `obj' as list."
  (channel-table->list (channels obj)))

(define (in-channel? obj chan)
  "Check if channel `chan' in irc-object `obj' is joined."
  (channel-ref (channels obj) chan))

(define (set-port! obj port)
  "If not yet connected change port to `port'."
  (if (connected? obj)
      (irc-error "set-port!: impossible to change port when connected.\n")
      (if (not (number? port))
	  (irc-type-error "set-port!" "number" port)
	  ((record-modifier irc-object 'port) obj port))))

(define (set-server! obj srv)
  "If not yet connected change server to `srv'."
  (if (connected? obj)
      (irc-error "set-server!: impossible to change server when connected.\n")
      (if (not (string? srv))
	  (irc-type-error "set-server!" "string" srv)
	  ((record-modifier irc-object 'server) obj srv))))

(define (set-realname! obj rn)
  "If not yet connected change realname to `rn'."
  (if (connected? obj)
      (irc-error "set-realname!: impossible to change realname when connected.\n")
      (if (not (string? rn))
	  (irc-type-error "set-realname!" "string" rn)
	  ((record-modifier irc-object 'realname) obj rn))))

(define (set-password! obj pwd)
  "If not yet connected change password to `pwd'."
  (if (connected? obj)
      (irc-error "set-password!: impossible to change password when connected.\n")
      (if (not (string? pwd))
	  (irc-type-error "set-password!" "string" pwd)
	  ((record-modifier irc-object 'password) obj pwd))))

(define (set-hostname! obj hn)
  "If not yet connected change hostname to `hn'."
  (if (connected? obj)
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
  (send-user))


(define (do-quit obj . msg)
  "Send QUIT to the server and clean up."
  (if (connected? obj)
      (begin (if (null? msg)
		 (send-message obj "QUIT :~a" *quitmsg*)
		 (send-message obj "QUIT :~a" msg))
	     (cleanup-irc-object obj)))
  (cleanup-irc-object obj))

(define (do-privmsg obj receiver msg)
  "Send message `msg' to `reciever' (channel or user)."
  (send-message obj "PRIVMSG ~a :~a" receiver msg))

(define* (do-command obj command #:optional msg)
  "Send command `command` to the server, with `msg' as optional string. The
return value is non specified."
  (send-message obj "~a ~a" (string-upcase command) msg))

(define (do-listen obj)
  "Return a parsed message (see the message module) if there is data available,
#f otherwise."
  (if (not (connected? obj))
      #f
      (let ([s (_socket obj)])
	(and (char-ready? s) (msg:make-message (read-line s))))))

(define (do-wait obj)
  (let loop ([msg (do-listen obj)])
    (if msg
	msg
	(begin (usleep 1000) (loop (do-listen obj))))))

(define (do-join obj chan)
  "Try to join channel `chan'."
  (send-message obj "JOIN ~a" chan)
  (channel-add! (channels obj) chan))

(define (do-part obj chan)
  "Part channel `chan'."
  (if (in-channel? obj chan)
      (send-message obj "PART ~a" chan)
      #f))

(define (do-runloop obj)
  ;; (if (not (or (exists-message-hook? obj 'ping)
  ;;	       (exists-message-hook? obj 'PING)))
  ;;     (install-ping-handler obj))
  (let ([sock (_socket obj)])
    (while (not (port-closed? sock))
      (run-message-hook obj (do-wait obj)))))

(define (set-filter! obj proc)
  "Use procedure `proc' as message filter. `proc' is called as (proc msg)
 and should return a new/modifier message or #f."
  (if (not (procedure? proc))
      (irc-type-error "install-filter!" 'proc "procedure" proc)
      ((record-modifier irc-object 'filter) obj proc)))

(define (reset-filter! obj)
  "Remove the installed filter."
  ((record-modifier irc-object 'filter) identity))

(define* (add-message-hook!
	  obj proc #:key tag append)
  "Install procedure `proc' as new message-hook. proc will be called as
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
	  (let ([prefix (msg:prefix msg)]
		[cmd (msg:command msg)]
		[param (msg:parameters msg)])
	    (let ([mid (msg:middle param)]
		  [trail (msg:trailing param)])
	      (and
	       (or (not sender)
		   (if (procedure? sender)
		       (sender prefix)
		       (string-contains prefix sender))
		   (string-contains prefix sender))
	       (or (not receiver)
		   (if (procedure? receiver)
		       (receiver middle)
		       (string-contains middle receiver)))
	       (or (not command)
		   (if (procedure? command)
		       (command cmd)
		       (eq? command cmd)))
	       (or (not trailing)
		   (if (procedure? trailing)
		       (trailing trail)
		       (string-contains trail trailing)))
	       (proc msg)))))])
    (add-tagged-hook! (hooks obj) handler tag append)))


(define (exists-message-hook? obj tag)
  "Returns #t if a hook with tag `tag' exists, #f otherwise."
  (if (find-tagged-hook (hooks obj) tag)
      #t
      #f))

(define (remove-message-hook! obj tag)
  "Remove all procedures in the hook that match tag `tag'."
  (remove-tagged-hook! (hooks obj) tag))

(define (run-message-hook obj . args)
  "Apply arguments `arg' to all the procedures in the hook."
  (apply run-tagged-hook (hooks obj) args))

(define (reset-message-hook! obj)
  "Remove all the message-hooks from irc-object `obj'."
  (reset-tagged-hook! (hooks obj)))

(define (install-ping-handler obj)
  (let ([ping-handler
	 (lambda (msg)
	   (do-command obj "PONG" (string-append
				   ":"
				   (msg:middle (msg:parameters msg)))))])
    (add-simple-message-hook! obj ping-handler #:tag 'ping #:command 'PING)))

(define* (install-printer obj #:key verbose (port (current-output-port)))
  (let ([printer
	 (if verbose
	     (lambda (msg)
	       (format port "raw: ~a\nprefix: ~a\ncommand: ~a\nparameters: ~a\n"
		       (msg:raw msg) (msg:prefix msg) (msg:command msg) (msg:parameters msg)))
	     (lambda (msg)
	       (format port "~a\n" (msg:raw msg))))])
    (add-message-hook! obj printer #:tag 'printer)))

(define (remove-printer obj)
  (remove-message-hook! obj 'printer))

(define (install-hello-handler obj)
  (let ([handler
	 (lambda (msg)
	   (let ([body (msg:trailing (msg:parameters msg))])
	     (if (and body (string-contains body ",hello"))
		 (do-privmsg obj (msg:middle (msg:parameters msg))
			     "hi master"))))])
    (add-simple-message-hook! obj handler #:tag 'hello #:command 'PRIVMSG)))
