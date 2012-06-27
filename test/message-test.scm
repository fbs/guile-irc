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

(define (print x) (begin (display x) (newline)))

(define-syntax expect
  (syntax-rules (not)
    ((_ fn e msg)
     (if (not (fn msg))
	 (throw 'error (make-error (list (quote fn) e) msg))))
    ((_ fn e1 e2 msg)
     (if (not (fn e1 e2))
	 (throw 'error (make-error (list (quote fn) e1 e2) msg))))))



(print ":server.org NOTICE Auth :*** Looking !")
(let ([m (msg:make-message ":server.org NOTICE Auth :*** Looking !")])
  (expect string=? (msg:prefix m) "server.org" "Testing prefix")
  (expect eq? (msg:command m) (string->symbol "NOTICE") "Testing command")
  (let ([p (msg:parameters m)])
    (expect string=? (msg:middle p) "Auth" "Testing middle")
    (expect string=? (msg:trailing p) "*** Looking !" "Testing tail")))
(print "Done with suite 1")
