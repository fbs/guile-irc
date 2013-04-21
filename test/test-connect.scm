#!/usr/bin/guile \ 
-e main -s
!#

(use-modules (irc irc)
             (irc handlers)
             (ice-9 rdelim))

(define (main args)
  (if (<  (length args) 2)
      (format #t "Usage: ./~a hostname port channel ssl(0/1)." (car args))
      (let ([a1 (cadr args)]
            [a2 (caddr args)]
            [a3 (cadddr args)]
            [a4 (cadr (cdddr args))])
        (let ([irc (make-irc #:nick "bot" 
                             #:port (string->number a2)
                             #:server a1
                             #:ssl (if (= 1 (string->number a4))
                                       #t
                                       #f))])
          (format #t "Connecting to ~a/~a channel ~a, ssl ~a" a1 a2 a3 a4)
          (install-ping-handler! irc)
          (install-hello-handler! irc)
          (install-printer! irc)
          (do-connect irc)
          ;; dirty hax till register gets fixed.
          (do-register irc)
          (sleep 2)
          (while (data-ready? irc)
            (run-message-hook irc (read-message irc)))
          (do-join irc a3)
          (sleep 1)
          (do-privmsg irc "#test" "Hello world! Use ,hello to test me.")
          (do-runloop irc)))))

