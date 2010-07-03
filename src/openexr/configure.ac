dnl Process this file with autoconf to produce a configure script.
AC_INIT(OpenEXR, 1.4.0)
AC_SUBST(OPENEXR_VERSION, 1.4.0)
AC_CANONICAL_HOST
AC_CONFIG_SRCDIR(ImathTest/main.cpp)
AM_CONFIG_HEADER(config/OpenEXRConfig.h)
AM_INIT_AUTOMAKE(1.6.3)  dnl Require automake 1.6.3 or better
AM_MAINTAINER_MODE


LIBTOOL_CURRENT=4
LIBTOOL_REVISION=0
LIBTOOL_AGE=0
LIBTOOL_VERSION=$LIBTOOL_CURRENT:$LIBTOOL_REVISION:$LIBTOOL_AGE
AC_SUBST(LIBTOOL_VERSION)

dnl Checks for programs.
AC_PROG_CXX
AC_PROG_INSTALL
AC_PROG_CC
AC_PROG_LN_S
AM_PROG_LIBTOOL
AC_PROG_MAKE_SET

dnl
dnl PKGCONFIG preparations
dnl

if test -z "${PKG_CONFIG_PATH}"; then
	PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/lib/pkgconfig
fi

export PKG_CONFIG_PATH

dnl
dnl check for alternate OpenGL headers
dnl
AM_PATH_GL(,)

dnl
dnl check for FLTK
dnl
AM_PATH_FLTK(have_fltk=yes, have_fltk=no)

dnl
dnl check for Cg
dnl
if test "${have_fltk}" = yes; then
  AM_PATH_CG(have_cg=yes, have_cg=no)
  if test "${have_cg}" = yes; then
    AC_DEFINE(HAVE_FRAGMENT_SHADERS)
  fi
else have_cg=no
fi

dnl Checks for libraries.
dnl --enable-threading
AC_ARG_ENABLE(threading,
              AC_HELP_STRING([--enable-threading],
                             [enable multi-threading [[default=yes]]]),
              [multithread="${enableval}"], [multithread=yes])

if test "${multithread}" != no ; then
    ACX_PTHREAD(
    [
	AC_DEFINE(HAVE_PTHREAD)
	LIBS="$PTHREAD_LIBS $LIBS"
	CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
	CXXFLAGS="$CXXFLAGS $PTHREAD_CFLAGS"
	CC="$PTHREAD_CC"
	
	AM_POSIX_SEM()
    ],
    [AC_MSG_ERROR([POSIX thread support required])])
fi

dnl Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS(limits.h unistd.h)

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_TYPE_SIZE_T

dnl Checks for library functions.
AC_CHECK_FUNCS(strerror)

dnl Checks for zlib
AC_CHECK_LIB(z, compress,
             [:],
             [AC_MSG_ERROR([
*** OpenEXR requires a recent version of zlib, which you don't appear to
*** have.
***
*** This could be because the run-time linker is not finding zlib, or it
*** is finding the wrong version.  In this case, you'll need to set your
*** LD_LIBRARY_PATH environment variable, or edit /etc/ld.so.conf to point
*** to the proper version.  Also, make sure you have run ldconfig if
*** that is required on your system.
			   ])]
)

dnl Checks for std::right etc. in iomanip
AC_MSG_CHECKING(for complete iomanip support in C++ standard library)
complete_iomanip="no"
AC_LANG_SAVE
AC_LANG_CPLUSPLUS
AC_TRY_COMPILE([#include <iomanip>],[

	std::right;
],
AC_DEFINE(HAVE_COMPLETE_IOMANIP) complete_iomanip=yes)
AC_MSG_RESULT($complete_iomanip)
AC_LANG_RESTORE


AC_MSG_CHECKING(for gcc optimization flags)
old_cflags=$CFLAGS
CFLAGS="$CFLAGS -pipe"
AC_TRY_COMPILE([#include <stdio.h>],
[ printf ("hello, world"); ],
[ EXTRA_OPT_CFLAGS="-pipe"],[ EXTRA_OPT_CFLAGS=""])
CFLAGS=$old_cflags
AC_MSG_RESULT([$EXTRA_OPT_CFLAGS])

dnl Platform-specific stuff
case "$host" in
*linux*)
  AC_DEFINE(HAVE_LINUX_PROCFS)
  ;;
*darwin*) 
  AC_DEFINE(HAVE_DARWIN) 

  dnl OS X universal binary support, requires --disable-dependency-tracking
  AC_ARG_ENABLE(osx-universal-binaries,
	      AC_HELP_STRING([--enable-osx-universal-binaries],
			     [build universal binaries on OS X [[default=no]]]),
	      [build_osxuniversal="${enableval}"], [build_osxuniversal=no])

  if test "${build_osxuniversal}" != no ; then
    if test "$enable_dependency_tracking" != no ; then
      AC_MSG_ERROR([--enable-osx-universal-binary requires --disable-dependency-tracking.
Please re-run configure with these options:
  --disable-dependency-tracking --enable-osx-universal-binary
  		  ])
    fi
    CXXFLAGS="$CXXFLAGS -isysroot /Developer/SDKs/MacOSX10.4u.sdk -arch ppc -arch i386"
    dnl LDFLAGS="$LDFLAGS -Wl,-syslibroot,/Developer/SDKs/MacOSX10.4u.sdk -arch ppc -arch i386"
  fi

  CXXFLAGS="$CXXFLAGS -Wno-long-double"
  ;;
esac

AM_CFLAGS="$EXTRA_OPT_CFLAGS"
AM_CXXFLAGS="$EXTRA_OPT_CFLAGS"

AC_SUBST(AM_CFLAGS)
AC_SUBST(AM_CXXFLAGS)

dnl build imfexamples example program?
build_imfexamples="no"
AC_ARG_ENABLE(imfexamples,
	      AC_HELP_STRING([--enable-imfexamples],
			     [build IlmImf example program [[default=no]]]),
	      [build_imfexamples="${enableval}"], [build_imfexamples=no])

AM_CONDITIONAL(HAVE_FLTK, test "x$have_fltk" = xyes)
AM_CONDITIONAL(HAVE_CG, test "x$have_cg" = xyes)
AM_CONDITIONAL(BUILD_IMFEXAMPLES, test "x$build_imfexamples" = xyes)

AC_OUTPUT([
Makefile
OpenEXR.pc
config/Makefile
Half/Makefile
HalfTest/Makefile
Iex/Makefile
IexTest/Makefile
IlmImf/Makefile
IlmImfTest/Makefile
IlmThread/Makefile
Imath/Makefile
ImathTest/Makefile
exrdisplay/Makefile
exrheader/Makefile
exrmaketiled/Makefile
IlmImfExamples/Makefile
doc/Makefile
vc/Makefile
exrstdattr/Makefile
exrmakepreview/Makefile
exrenvmap/Makefile
])

if test "x$build_osxuniversal" = xyes; then
  AC_MSG_RESULT([
---------------------------------------------
Summary for OpenEXR features:

build exrdisplay FLTK application		$have_fltk
    with fragment shader support		$have_cg
build IlmImf example program                    $build_imfexamples
build OS X universal binaries                   $build_osxuniversal
---------------------------------------------
  ])
else
  AC_MSG_RESULT([
---------------------------------------------
Summary for OpenEXR features:

build exrdisplay FLTK application		$have_fltk
    with fragment shader support		$have_cg
build IlmImf example program                    $build_imfexamples
---------------------------------------------
  ])
fi

