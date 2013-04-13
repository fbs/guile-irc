include guile.am

moddir=$(prefix)/share/guile/site/2.0
godir=$(libdir)/guile/2.0/site-ccache

SOURCES = \
	irc/config.scm \
	irc/channel.scm \
	irc/error.scm \
	irc/error-code.scm \
	irc/message.scm \
	irc/tagged-hook.scm \
	irc/irc.scm \
	irc/handlers.scm

EXTRA_DIST += env.in

info_TEXINFOS = doc/api.texi

TESTS = \
	test/message-test.scm
TESTS_ENVIRONMENT = $(top_builddir)/env $(GUILE) --no-auto-compile
EXTRA_DIST += $(TESTS) $(info_TEXINFOS)
