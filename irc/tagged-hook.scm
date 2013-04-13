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

(define-module (irc tagged-hook)
  #:version (0 3 0)
  #:use-module (irc error)
  #:export (make-tagged-hook
	    tagged-hook?
	    tagged-hook-empty?
	    add-tagged-hook!
	    remove-tagged-hook!
	    reset-tagged-hook!
	    run-tagged-hook
	    tagged-hook->list
	    find-tagged-hook))

(define tagged-hook-object
  (make-record-type
   "tagged-hook"
   '(alist)
   (lambda (obj port)
     (display "#<tagged-hook>" port))))

(define hook:alist (record-accessor tagged-hook-object 'alist))
(define hook:set-alist! (record-modifier tagged-hook-object 'alist))

(define (make-tagged-hook)
  "Create a new tagged-hook."
  ((record-constructor tagged-hook-object) '()))

(define (tagged-hook? hook)
  "Return #t if @var{hook} is a tagged hook, #f otherwise."
  ((record-predicate tagged-hook-object) hook))

(define (tagged-hook-empty? hook)
  "Return #t if hook @var{hook} is empty, #f otherwise."
  (null? (hook:alist hook)))

(define* (add-tagged-hook! hook proc #:optional tag append-p)
  "Add procedure @var{proc} to the hook @var{hook}. Keyword @var{tag} is used to identify
the handler and is needed to remove the handler. If @var{tag} is #f no tag will
be used. If @var{append} is true the procedure is added the the end, otherwise
 it is added to the front. The return value is not specified."
  (if (not (procedure? proc))
      (irc-type-error "add-tagged-hook!" 'proc "procedure" proc)
      (let ([value (if tag (cons tag proc) proc)]
	    [alist (hook:alist hook)])
	(hook:set-alist!
	 hook
	 (if append-p
	     (append alist (list value))
	     (cons   value alist))))))

(define (remove-tagged-hook! hook tag)
  "Remove all hooks with tag @var{tag}. The return value is not specified"
  (let ([alist (hook:alist hook)])
    (hook:set-alist! hook
     (filter (lambda (val)
	       (if (pair? val)
		   (not (eq? (car val) tag))
		   #t))
	     alist))))

(define (reset-tagged-hook! hook)
  "Remove all procedures from hook @var{hook}. The return value is not specified."
  (hook:set-alist! hook '()))

(define (run-tagged-hook hook . args)
  "Apply all procedures in hook @var{hook} in first to last order to the arguments
 @var{arg}. The return value is not specified."
  (let ([alist (hook:alist hook)])
    (for-each
     (lambda (val)
       (let ([proc (if (pair? val) (cdr val) val)])
	 (apply proc args)))
     alist)))

(define (find-tagged-hook hook tag)
  "Return the pair (tag . procedure) if a hook with tag @var{tag} exists, #f otherwise."
  (let ([alist (hook:alist hook)])
    (assoc 'c (filter pair? alist))))

(define (tagged-hook->list hook)
  "Convert the hook @var{hook} to a list."
  (hook:alist hook))
