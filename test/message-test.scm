#! /usr/bin/guile -s
!#

;; Copyright (C) 2012 bas smit (fbs)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

;(add-to-load-path "/home/bas/projects/programming/lib-guile/")

(use-modules ((irc message)
	      #:renamer (symbol-prefix-proc 'msg:)))

(define make-error
  (lambda (expr msg)
    (cons expr msg)))

(define error-expression car)
(define error-message cdr)

(define (print . x)
  (if (not (list? x))
      (newline)
      (for-each (lambda (y) (begin (display y) (newline))) x)))

(define-syntax expect
  (syntax-rules ()
    ((_ fn e msg)
     (if (not (fn e))
	 (throw 'error (make-error (list (quote fn) e) msg))))
    ((_ fn e1 e2 msg)
     (if (not (fn e1 e2))
	 (throw 'error (make-error (list (quote fn) e1 e2) msg))))))

(define-syntax test
  (syntax-rules ()
    ((_ fn e msg)
     (if (fn e)
	 (format #t "good: ~a\n" msg)
	 (format #t "bad:  ~a\n" msg)))
    ((_ fn e1 e2 msg)
     (if (fn e1 e2)
	 (format #t "good: ~a\n" msg)
	 (format #t "bad:  ~a\n" msg)))))

(print  "Running test 1")
(let ([m (msg:parse-message-string ":server.org NOTICE Auth :*** Looking !")])
  (test string=? (msg:prefix m) "server.org" "prefix test")
  (test eq? (msg:command m) (string->symbol "NOTICE") "command test")
  (test number? (msg:time m) "timestamp test")
  (test string=? (msg:middle m) "Auth" "middle test")
  (test string=? (msg:trailing m) "*** Looking !" "tail test"))
(print "Done with test 1") (newline)

(print "Running test 2")
(let ([m (msg:parse-message-string ":moorcock.freenode.net 001 foeps :Welcome !@#$%^&*()-=_+[]{};';\",./<>?")])
  (test string=? (msg:prefix m) "moorcock.freenode.net" "prefix test")
  (test = (msg:command m) 1 "command test")
  (test number? (msg:time m) "timestamp test")
  (test <= (msg:time m) (current-time) "timestamp test")
  (test string=? (msg:middle m) "foeps" "middle test")
  (test string=? (msg:trailing m) "Welcome !@#$%^&*()-=_+[]{};';\",./<>?" "tail test"))
(print "Done with test 2") (newline)


(define str1 ":moorcock.freenode.net NOTICE * :*** Looking up your hostname...")
(define str2 ":moorcock.freenode.net 003 foeps :This server was created Tue Feb 7 2012 at 15:05:50 CST")
(define str3 ":irc.baslab.bas 366 fubs #test :End of /NAMES list.")
(define str4 ":bas!bas@127.0.0.1 JOIN :#test")
(define str5 ":bas!bas@127.0.0.1 PRIVMSG #test :hello world")
(define str6 ":bas!bas@127.0.0.1 NOTICE #test :hello")
(define str7 ":bas!bas@127.0.0.1 PRIVMSG fubs :VERSION")
(define str8 ":fubs!fubs@127.0.0.1 MODE #test +o bas")
(define str9 ":bas!bas@127.0.0.1 MODE #test +v fubs")
(define str10 ":bas!bas@127.0.0.1 KICK #test fubs :")
