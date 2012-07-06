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

(define-module (irc channel)
  #:version (0 0)
  #:export (make-channel-table
	    channel-add!
	    channel-remove!
	    channel-clear!
	    channel-table->list
	    channel-ref))

(define (make-channel-table)
  (make-hash-table 31))

(define (channel-add! table channel)
  (hash-set! table channel #t))

(define (channel-remove! table channel)
  (hash-remove! table channel))

(define channel-clear! hash-clear!)

(define (channel-table->list table)
  (hash-map->list (lambda (x y) x) table))

(define (channel-ref table channel)
  (hash-ref table channel))
