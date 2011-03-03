dnl @synopsis XERCES_PRETTY_MAKE
dnl
dnl Determines the whether we're doing a pretty make
dnl --enable-pretty-make=yes is the default
dnl
dnl @category Xerces
dnl @author Axel Weiss and James Berry
dnl @version 2005-05-27
dnl @license AllPermissive
dnl
dnl $Id: xerces_pretty_make.m4 678145 2008-07-19 12:10:43Z borisk $

AC_DEFUN([XERCES_PRETTY_MAKE],
	[
	AC_MSG_CHECKING([whether we'll generate prettier make output])
	AC_ARG_ENABLE([pretty-make],
		AS_HELP_STRING([--disable-pretty-make],
			[Disable prettier make output (enabled by default)]),
		[AS_IF([test x"$enableval" = xyes],
			[xerces_pretty_make=yes],
			[xerces_pretty_make=no])],
		[xerces_pretty_make=yes])
	AC_MSG_RESULT([$xerces_pretty_make])
	
	# Define the auto-make conditionals which determine what actually gets compiled
	# Note that these macros can't be executed conditionally, which is why they're here, not above.
	AM_CONDITIONAL([XERCES_PRETTY_MAKE], [test x"$xerces_pretty_make" = xyes])

	]
)
