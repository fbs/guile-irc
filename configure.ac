dnl -*- Autoconf -*-

AC_INIT(guile-irc, 0.2)
AC_CONFIG_SRCDIR(irc/irc.scm)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])		

GUILE_PROGS

if $GUILE_TOOLS | grep -q compile; then
   # everything cool
   true
else
   AC_MSG_ERROR([Guile 2.0 required.])
fi

AC_CONFIG_FILES([Makefile] [doc/Makefile])
AC_CONFIG_FILES([env], [chmod +x env])


AC_CHECK_PROG(have_makeinfo, makeinfo, yes, no)
AM_CONDITIONAL(HAVE_MAKEINFO, test "$have_makeinfo" = yes)

AC_CHECK_PROG(have_pdf, texi2pdf, yes, no)
AM_CONDITIONAL(HAVE_PDF, test "$have_pdf" = yes)

AC_OUTPUT
