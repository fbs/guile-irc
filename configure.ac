AC_PREREQ ([2.50])
AC_INIT([guile-irc], 0.2)
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_SRCDIR([irc.scm])
AM_SILENT_RULES([yes])		

AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE()

GUILE_PROGS()
GUILE_SITE_DIR()

PKG_CHECK_MODULES([GUILE], [guile-2.0])

AC_CHECK_PROG(have_makeinfo, makeinfo, yes, no)
AM_CONDITIONAL(HAVE_MAKEINFO, test "$have_makeinfo" = yes)

AC_CHECK_PROG(have_pdf, texi2pdf, yes, no)
AM_CONDITIONAL(HAVE_PDF, test "$have_pdf" = yes)

AC_CONFIG_FILES([Makefile] [doc/Makefile])

AC_OUTPUT()
