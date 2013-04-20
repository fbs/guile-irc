#!/usr/bin/guile -s
!#

(use-modules (irc irc)
             (irc handlers))

(define irc (make-irc #:port 6667 #:server "localhost"))

(begin (install-ping-handler! irc) (install-hello-handler! irc) (install-printer! irc))
(do-connect irc)
(do-register irc)
(sleep 5)
(do-join irc "#test")
(sleep 1)
(do-privmsg irc "#test" "Hello world! Use ,hello to test me.")
(do-runloop irc)
