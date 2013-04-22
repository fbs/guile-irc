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

(define-module (irc error)
  #:version (0 3 0)
  #:export (irc-error
	    irc-type-error))

(define-syntax irc-error
  (syntax-rules ()
    ((_ msg ...)
     (throw 'irc-error (format #f msg ...)))))

(define-syntax irc-type-error
  (syntax-rules ()
    ((_ origin arg expected got)
     (throw 'irc-type-error (format #f "~a: argument ~a: expected type ~a, got ~a"
				    origin arg expected got)))
    ((_ origin expected got)
     (throw 'irc-type-error (format #f "~a: expected type ~a, got ~a"
				    origin expected got)))))
