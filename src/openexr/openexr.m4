# aclocal.m4 generated automatically by aclocal 1.6.3 -*- Autoconf -*-

# Copyright 1996, 1997, 1998, 1999, 2000, 2001, 2002
# Free Software Foundation, Inc.
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.

dnl
dnl OpenEXR
dnl

AC_DEFUN([AM_PATH_OPENEXR],
[dnl 
dnl Get the cflags and libraries
dnl
AC_ARG_WITH(openexr-prefix,[  --with-openexr-prefix=PFX   Prefix where libopenexr is installed (optional)], openexr_prefix="$withval", openexr_prefix="/usr")
AC_ARG_ENABLE(openexrtest, [  --disable-openexrtest       Do not try to compile and run a test Openexr program],, enable_openexrtest=yes)

  if test "x$openexr_prefix" != "xNONE" ; then
    openexr_args="$openexr_args --prefix=$openexr_prefix"
    OPENEXR_INCLUDES="-I$openexr_prefix/include/OpenEXR"
    OPENEXR_LIBS="-L$openexr_prefix/lib"
  elif test "$prefix" != ""; then
    openexr_args="$openexr_args --prefix=$prefix"
    OPENEXR_INCLUDES="-I$prefix/include/OpenEXR"
    OPENEXR_LIBS="-L$prefix/lib"
  fi

  OPENEXR_LIBS="$X_LIBS $OPENEXR_LIBS -lIlmImf -lImath -lIex -lHalf -lz"

  AC_MSG_CHECKING(for OpenEXR)
  no_openexr=""


  if test "x$enable_openexrtest" = "xyes" ; then
    ac_save_CXXFLAGS="$CXXFLAGS"
    ac_save_LIBS="$LIBS"
    CXXFLAGS="$CXXFLAGS $OPENEXR_CXXFLAGS $OPENEXR_INCLUDES"
    LIBS="$LIBS $OPENEXR_LIBS"
      rm -f conf.openexrtest
      AC_LANG_SAVE
      AC_LANG_CPLUSPLUS
      AC_TRY_RUN([
#include <stdlib.h>
#include <ImfRgbaFile.h>

int main ()
{
  try
  {
    Imf::RgbaInputFile exr ("");
  }
  catch (...)
  {
  }
  system("touch conf.openexrtest");
  return 0;
}

],, no_openexr=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
       AC_LANG_RESTORE
       CXXFLAGS="$ac_save_CXXFLAGS"
       LIBS="$ac_save_LIBS"
  fi

  if test "x$no_openexr" = "x" ; then
     AC_MSG_RESULT(yes)
     ifelse([$1], , :, [$1])     
  else
     AC_MSG_RESULT(no)
     if test -f conf.openexrtest ; then
       :
     else
       echo "*** Could not run OpenEXR test program, checking why..."
       CXXFLAGS="$CXXFLAGS $OPENEXR_CXXFLAGS $OPENEXR_INCLUDES"
       LIBS="$LIBS $OPENEXR_LIBS"
       AC_LANG_SAVE
       AC_LANG_CPLUSPLUS
       AC_TRY_LINK([
#include <stdio.h>
#include <ImfRgbaFile.h>
],     [ Imf::RgbaInputFile exr ("");return 0; ],
       [ echo "*** The test program compiled, but did not run. This usually means"
       echo "*** that the run-time linker is not finding OpenEXR or finding the wrong"
       echo "*** version of OpenEXR. If it is not finding OpenEXR, you'll need to set your"
       echo "*** LD_LIBRARY_PATH environment variable, or edit /etc/ld.so.conf to point"
       echo "*** to the installed location  Also, make sure you have run ldconfig if that"
       echo "*** is required on your system"
       echo "***"])
       AC_LANG_RESTORE
       CXXFLAGS="$ac_save_CXXFLAGS"
       LIBS="$ac_save_LIBS"
     fi
     OPENEXR_INCLUDES=""
     OPENEXR_LIBS=""
     ifelse([$2], , :, [$2])
  fi
  AC_SUBST(OPENEXR_INCLUDES)
  AC_SUBST(OPENEXR_LIBS)
  rm -f conf.openexrtest
])
