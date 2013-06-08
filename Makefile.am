include guile.am

moddir=$(prefix)/share/guile/site/2.0
godir=$(libdir)/guile/2.0/site-ccache

SOURCES = \
	irc/channel.scm \
	irc/error.scm \
	irc/error-code.scm \
	irc/message.scm \
	irc/tagged-hook.scm \
	irc/irc.scm \
	irc/handlers.scm
if GNUTLS
SOURCES += \
	irc/gnutls.scm \
	irc/network.scm
else
SOURCES += 
endif

info_TEXINFOS = doc/api.texi

TESTS = \
	test/message-test.scm
TESTS_ENVIRONMENT = $(GUILE) --no-auto-compile
EXTRA_DIST += $(TESTS) $(info_TEXINFOS)
