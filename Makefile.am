GUILE_FLAGS = -Wunbound-variable -Warity-mismatch -Wformat

SCM_SOURCES = 		\
	channel.scm     \
	error-code.scm  \
	error.scm       \
	handlers.scm    \
	irc.scm	        \
	message.scm     \
	tagged-hook.scm 

GOBJECTS		 = $(SCM_SOURCES:%.scm=%.go)

scmdir			 = $(GUILE_SITE)/irc/
godir			 = $(libdir)/guile/20/ccache/irc/

nobase_scm_DATA		 = $(SCM_SOURCES)
nobase_go_DATA		 = $(GOBJECTS) 

guile_install_go_files	 = install-nobase_goDATA
$(guile_install_go_files): install-nobase_scmDATA

SUFFIXES		 = .scm .go
CLEANFILES		 = $(GOBJECTS)

SUBDIRS = doc
AUTOMAKE_OPTIONS = subdir-objects

.scm.go:
	$(GUILE_TOOLS) compile $(GUILE_FLAGS) -o "$@" "$<"
