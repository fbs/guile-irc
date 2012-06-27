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

;; with some help with dsmith from #guile

(define-module (irc message)
  #:version (0 0)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:export (make-message
	    message?
	    prefix
	    command
	    parameters
	    time
	    raw
	    middle
	    trailing
	    symbolize
	    make-parser test-irc-msg))

;; <message> ::=
;;     [':' <prefix> <SPACE> ] <command> <params> <crlf>
;; <prefix> ::=
;;     <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
;; <command> ::=
;;     <letter> { <letter> } | <number> <number> <number>
;; <SPACE> ::=
;;     ' ' { ' ' }
;; <params> ::=
;;     <SPACE> [ ':' <trailing> | <middle> <params> ]
;; <middle> ::=
;;     <Any *non-empty* sequence of octets not including SPACE or NUL or CR or LF, the first of which may not be ':'>
;; <trailing> ::=
;;     <Any, possibly *empty*, sequence of octets not including NUL or CR or LF>
;; <crlf> ::=
;;     CR LF

(define test-irc-msg ":test!test@123.456.789.234.spam.dump.nl PRIVMSG #bot :spam")

(define middle car)
(define trailing cadr)

(define message-object (make-record-type "irc:message"
					 '(prefix
					   command
					   parameters
					   time
					   raw)))

(define message? (record-predicate message-object))
(define prefix (record-accessor message-object 'prefix))
(define command (record-accessor message-object 'command))
(define parameters (record-accessor message-object 'parameters))
(define time  (record-accessor message-object 'time))
(define raw  (record-accessor message-object 'raw))

(define* (make-message-object #:key prefix command parameters time raw)
  ((record-constructor message-object)
   prefix
   command
   parameters
   time
   raw))

(define (make-parser)
  (let ([rx1 (make-regexp "^(:([^ ]+) +)?([^ ]+) +(.+)$")]
	[rx2 (make-regexp "^([^:]*):?(.+)?$")])
    (lambda (msg)
      (catch #t
	(lambda ()
	  (let* ([m1 (regexp-exec rx1 msg)]
		 [m2 (regexp-exec rx2 (match:substring m1 4))]
		 [params (delete #f
				 (append (string-tokenize (match:substring m2 1))
					 (list (match:substring m2 2))))])
	    (make-message-object #:prefix      (match:substring m1 2)
				 #:command     (symbolize (match:substring m1 3))
				 #:parameters  params
				 #:time        (current-time)
				 #:raw         msg)))
	(lambda (key . args) (throw 'irc:msg:error (format #f "UNHANDLED: ~a" msg)))))))

(define (symbolize cmd)
  (if (char-numeric? (string-ref cmd 0))
      (string->number cmd)
      (string->symbol cmd)))

(define make-message (make-parser))
