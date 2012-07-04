# GUILE-IRC

# Usage
    (define con (make-irc-object))
    (install-ping-handler con)
    (install-printer con #:verbose #t)
    (set-server! con "localhost")
    (set-port! con 6667)
    (do-connect con)
    (do-register con)
    (do-join con "#test")
    (do-runloop con)
