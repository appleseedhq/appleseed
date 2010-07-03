dnl
dnl Alternate OpenGL headers (e.g. for Nvidia headers)
dnl

AC_DEFUN([AM_PATH_GL],
[dnl
dnl Get the cflags
dnl
AC_ARG_WITH(gl-includes,[  --with-gl-includes=PFX  Specify which OpenGL headers to use],
	gl_includes="$withval", gl_includes="")
  if test x$gl_includes != x ; then
    GL_CXXFLAGS="-I$gl_includes"
  else
    GL_CXXFLAGS=""
  fi

  AC_SUBST(GL_CXXFLAGS)
])



dnl
dnl Cg support
dnl

AC_DEFUN([AM_PATH_CG],
[dnl
dnl Get the cflags
dnl
AC_ARG_WITH(cg-prefix,[  --with-cg-prefix=PFX  Prefix where Cg is installed (optional)],
	    cg_prefix="$withval", cg_prefix="")

  if test x$cg_prefix != x ; then
    CG_CXXFLAGS="-I$cg_prefix/include"
    CG_LDFLAGS="-L$cg_prefix/lib -lGL -lCg -lCgGL -lGLU -lpthread"
  else
    CG_CXXFLAGS=""
    CG_LDFLAGS="-lGL -lCg -lCgGL -lGLU -lpthread"
  fi

  AC_MSG_CHECKING(for Cg)
  no_cg=""

  ac_save_CXXFLAGS="$CXXFLAGS"
  ac_save_LDFLAGS="$LDFLAGS"
  CXXFLAGS="$CXXFLAGS $CG_CXXFLAGS"
  LDFLAGS="$CG_LDFLAGS"

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  AC_TRY_LINK([
#include <GL/gl.h>
#include <Cg/cg.h>
#include <Cg/cgGL.h>],
[
    cgCreateContext ();
],, no_cg=yes)
  AC_LANG_RESTORE
  CXXFLAGS="$ac_save_CXXFLAGS"
  LDFLAGS="$ac_save_LDFLAGS"

  if test "x$no_cg" = "x" ; then
    AC_MSG_RESULT(yes)
      ifelse([$1], , :, [$1])
  else
    AC_MSG_RESULT(no)
    echo "*** The Cg test program could not be compiled."
    echo "*** Possible reasons:"
    echo "***     - The Cg libraries and includes are not installed."
    echo "***     - configure cannot find Cg (use the"
    echo "***       --with-cg-prefix option to tell configure where"
    echo "***       to find it)."
    echo "***     - Your version of Cg is out of date.  Please update it"
    echo "***       to the latest version."
    echo "***"
    echo "*** The exrdisplay program will not be built with fragment shader"
    echo "*** support because the fragment shader support depends on Cg."
    CG_CXXFLAGS=""
    CG_LDFLAGS=""
    ifelse([$2], , :, [$2])
  fi
  AC_SUBST(CG_CXXFLAGS)
  AC_SUBST(CG_LDFLAGS)
])
  

dnl
dnl FLTK with GL support
dnl

AC_DEFUN([AM_PATH_FLTK],
[dnl 
dnl Get the cflags and libraries
dnl
AC_ARG_VAR(FLTK_CONFIG, Path to fltk-config command)
AC_PATH_PROG(FLTK_CONFIG, fltk-config, no)
AC_ARG_WITH(fltk-config,[  --with-fltk-config=PATH Specify which fltk-config to use (optional)], FLTK_CONFIG="$withval",)

  if test x$FLTK_CONFIG != xno ; then
    FLTK_CXXFLAGS="`$FLTK_CONFIG --use-gl --cxxflags`"
    FLTK_LDFLAGS="`$FLTK_CONFIG --use-gl --ldflags`"
  else
    FLTK_CXXFLAGS=""
    FLTK_LDFLAGS=""
  fi

  AC_MSG_CHECKING(for FLTK with GL support)
  no_fltk=""

  ac_save_CXXFLAGS="$CXXFLAGS"
  ac_save_LDFLAGS="$LDFLAGS"
  CXXFLAGS="$CXXFLAGS $FLTK_CXXFLAGS"
  LDFLAGS="$LDFLAGS $FLTK_LDFLAGS"

dnl
dnl Now check if the installed FLTK has GL support
dnl
  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  AC_TRY_LINK([
#include <stdlib.h>
#include <FL/Fl.H>
#include <FL/Fl_Gl_Window.H>],
[
    Fl_Gl_Window foo (); 
],, no_fltk=yes)
  AC_LANG_RESTORE
  CXXFLAGS="$ac_save_CXXFLAGS"
  LDFLAGS="$ac_save_LDFLAGS"
  
  if test "x$no_fltk" = "x" ; then
    AC_MSG_RESULT(yes)
     ifelse([$1], , :, [$1])     
  else
    AC_MSG_RESULT(no)
    echo "*** The fltk test program could not be compiled.  Possible reasons:"
    echo "***"
    echo "***     - FLTK is not installed."
    echo "***     - Your version of FLTK does not support OpenGL."
    echo "***     - configure cannot find your 'fltk-config' program (use"
    echo "***       the --with-fltk-config option to tell configure where"
    echo "***       to find it)."
    echo "***     - Your version of FLTK is too old.  The exrdisplay"
    echo "***       program requires FLTK 1.1 or higher."
    echo "***     - Your FLTK library was compiled with a different C++"
    echo "***       compiler than the one you're using to compile OpenEXR."
    echo "***"
    echo "*** The exrdisplay program will not be built because it depends on"
    echo "*** a working FLTK install with OpenGL support."
    FLTK_CXXFLAGS=""
    FLTK_LDFLAGS=""
    ifelse([$2], , :, [$2])
  fi
  AC_SUBST(FLTK_CXXFLAGS)
  AC_SUBST(FLTK_LDFLAGS)
])


dnl @synopsis ACX_PTHREAD([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl Modified by Wojciech Jarosz (2005) to include check for POSIX
dnl semaphore usability. Defines HAVE_POSIX_SEMAPHORES if found.
dnl
dnl This macro figures out how to build C programs using POSIX threads.
dnl It sets the PTHREAD_LIBS output variable to the threads library and
dnl linker flags, and the PTHREAD_CFLAGS output variable to any special
dnl C compiler flags that are needed. (The user can also force certain
dnl compiler flags/libs to be tested by setting these environment
dnl variables.)
dnl
dnl Also sets PTHREAD_CC to any special C compiler that is needed for
dnl multi-threaded programs (defaults to the value of CC otherwise).
dnl (This is necessary on AIX to use the special cc_r compiler alias.)
dnl
dnl NOTE: You are assumed to not only compile your program with these
dnl flags, but also link it with them as well. e.g. you should link
dnl with $PTHREAD_CC $CFLAGS $PTHREAD_CFLAGS $LDFLAGS ... $PTHREAD_LIBS
dnl $LIBS
dnl
dnl If you are only building threads programs, you may wish to use
dnl these variables in your default LIBS, CFLAGS, and CC:
dnl
dnl        LIBS="$PTHREAD_LIBS $LIBS"
dnl        CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
dnl        CC="$PTHREAD_CC"
dnl
dnl In addition, if the PTHREAD_CREATE_JOINABLE thread-attribute
dnl constant has a nonstandard name, defines PTHREAD_CREATE_JOINABLE to
dnl that name (e.g. PTHREAD_CREATE_UNDETACHED on AIX).
dnl
dnl ACTION-IF-FOUND is a list of shell commands to run if a threads
dnl library is found, and ACTION-IF-NOT-FOUND is a list of commands to
dnl run it if it is not found. If ACTION-IF-FOUND is not specified, the
dnl default action will define HAVE_PTHREAD.
dnl
dnl Please let the authors know if this macro fails on any platform, or
dnl if you have any other suggestions or comments. This macro was based
dnl on work by SGJ on autoconf scripts for FFTW (www.fftw.org) (with
dnl help from M. Frigo), as well as ac_pthread and hb_pthread macros
dnl posted by Alejandro Forero Cuervo to the autoconf macro repository.
dnl We are also grateful for the helpful feedback of numerous users.
dnl
dnl @category InstalledPackages
dnl @author Steven G. Johnson <stevenj@alum.mit.edu>
dnl @version 2005-01-14
dnl @license GPLWithACException

AC_DEFUN([ACX_PTHREAD], [
AC_REQUIRE([AC_CANONICAL_HOST])
AC_LANG_SAVE
AC_LANG_C
acx_pthread_ok=no

# We used to check for pthread.h first, but this fails if pthread.h
# requires special compiler flags (e.g. on True64 or Sequent).
# It gets checked for in the link test anyway.

# First of all, check if the user has set any of the PTHREAD_LIBS,
# etcetera environment variables, and if threads linking works using
# them:
if test x"$PTHREAD_LIBS$PTHREAD_CFLAGS" != x; then
        save_CFLAGS="$CFLAGS"
        CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
        save_LIBS="$LIBS"
        LIBS="$PTHREAD_LIBS $LIBS"
        AC_MSG_CHECKING([for pthread_join in LIBS=$PTHREAD_LIBS with CFLAGS=$PTHREAD_CFLAGS])
        AC_TRY_LINK_FUNC(pthread_join, acx_pthread_ok=yes)
        AC_MSG_RESULT($acx_pthread_ok)
        if test x"$acx_pthread_ok" = xno; then
                PTHREAD_LIBS=""
                PTHREAD_CFLAGS=""
        fi
        LIBS="$save_LIBS"
        CFLAGS="$save_CFLAGS"
fi

# We must check for the threads library under a number of different
# names; the ordering is very important because some systems
# (e.g. DEC) have both -lpthread and -lpthreads, where one of the
# libraries is broken (non-POSIX).

# Create a list of thread flags to try.  Items starting with a "-" are
# C compiler flags, and other items are library names, except for "none"
# which indicates that we try without any flags at all, and "pthread-config"
# which is a program returning the flags for the Pth emulation library.

acx_pthread_flags="pthreads none -Kthread -kthread lthread -pthread -pthreads -mthreads pthread --thread-safe -mt pthread-config"

# The ordering *is* (sometimes) important.  Some notes on the
# individual items follow:

# pthreads: AIX (must check this before -lpthread)
# none: in case threads are in libc; should be tried before -Kthread and
#       other compiler flags to prevent continual compiler warnings
# -Kthread: Sequent (threads in libc, but -Kthread needed for pthread.h)
# -kthread: FreeBSD kernel threads (preferred to -pthread since SMP-able)
# lthread: LinuxThreads port on FreeBSD (also preferred to -pthread)
# -pthread: Linux/gcc (kernel threads), BSD/gcc (userland threads)
# -pthreads: Solaris/gcc
# -mthreads: Mingw32/gcc, Lynx/gcc
# -mt: Sun Workshop C (may only link SunOS threads [-lthread], but it
#      doesn't hurt to check since this sometimes defines pthreads too;
#      also defines -D_REENTRANT)
# pthread: Linux, etcetera
# --thread-safe: KAI C++
# pthread-config: use pthread-config program (for GNU Pth library)

case "${host_cpu}-${host_os}" in
        *solaris*)

        # On Solaris (at least, for some versions), libc contains stubbed
        # (non-functional) versions of the pthreads routines, so link-based
        # tests will erroneously succeed.  (We need to link with -pthread or
        # -lpthread.)  (The stubs are missing pthread_cleanup_push, or rather
        # a function called by this macro, so we could check for that, but
        # who knows whether they'll stub that too in a future libc.)  So,
        # we'll just look for -pthreads and -lpthread first:

        acx_pthread_flags="-pthread -pthreads pthread -mt $acx_pthread_flags"
        ;;
esac

if test x"$acx_pthread_ok" = xno; then
for flag in $acx_pthread_flags; do

        case $flag in
                none)
                AC_MSG_CHECKING([whether pthreads work without any flags])
                ;;

                -*)
                AC_MSG_CHECKING([whether pthreads work with $flag])
                PTHREAD_CFLAGS="$flag"
                ;;

		pthread-config)
		AC_CHECK_PROG(acx_pthread_config, pthread-config, yes, no)
		if test x"$acx_pthread_config" = xno; then continue; fi
		PTHREAD_CFLAGS="`pthread-config --cflags`"
		PTHREAD_LIBS="`pthread-config --ldflags` `pthread-config --libs`"
		;;

                *)
                AC_MSG_CHECKING([for the pthreads library -l$flag])
                PTHREAD_LIBS="-l$flag"
                ;;
        esac

        save_LIBS="$LIBS"
        save_CFLAGS="$CFLAGS"
        LIBS="$PTHREAD_LIBS $LIBS"
        CFLAGS="$CFLAGS $PTHREAD_CFLAGS"

        # Check for various functions.  We must include pthread.h,
        # since some functions may be macros.  (On the Sequent, we
        # need a special flag -Kthread to make this header compile.)
        # We check for pthread_join because it is in -lpthread on IRIX
        # while pthread_create is in libc.  We check for pthread_attr_init
        # due to DEC craziness with -lpthreads.  We check for
        # pthread_cleanup_push because it is one of the few pthread
        # functions on Solaris that doesn't have a non-functional libc stub.
        # We try pthread_create on general principles.
        AC_TRY_LINK([#include <pthread.h>],
                    [pthread_t th; pthread_join(th, 0);
                     pthread_attr_init(0); pthread_cleanup_push(0, 0);
                     pthread_create(0,0,0,0); pthread_cleanup_pop(0); ],
                    [acx_pthread_ok=yes])

        LIBS="$save_LIBS"
        CFLAGS="$save_CFLAGS"

        AC_MSG_RESULT($acx_pthread_ok)
        if test "x$acx_pthread_ok" = xyes; then
                break;
        fi

        PTHREAD_LIBS=""
        PTHREAD_CFLAGS=""
done
fi

# Various other checks:
if test "x$acx_pthread_ok" = xyes; then
        save_LIBS="$LIBS"
        LIBS="$PTHREAD_LIBS $LIBS"
        save_CFLAGS="$CFLAGS"
        CFLAGS="$CFLAGS $PTHREAD_CFLAGS"

        # Detect AIX lossage: JOINABLE attribute is called UNDETACHED.
	AC_MSG_CHECKING([for joinable pthread attribute])
	attr_name=unknown
	for attr in PTHREAD_CREATE_JOINABLE PTHREAD_CREATE_UNDETACHED; do
	    AC_TRY_LINK([#include <pthread.h>], [int attr=$attr;],
                        [attr_name=$attr; break])
	done
        AC_MSG_RESULT($attr_name)
        if test "$attr_name" != PTHREAD_CREATE_JOINABLE; then
            AC_DEFINE_UNQUOTED(PTHREAD_CREATE_JOINABLE, $attr_name,
                               [Define to necessary symbol if this constant
                                uses a non-standard name on your system.])
        fi

        AC_MSG_CHECKING([if more special flags are required for pthreads])
        flag=no
        case "${host_cpu}-${host_os}" in
            *-aix* | *-freebsd* | *-darwin*) flag="-D_THREAD_SAFE";;
            *solaris* | *-osf* | *-hpux*) flag="-D_REENTRANT";;
        esac
        AC_MSG_RESULT(${flag})
        if test "x$flag" != xno; then
            PTHREAD_CFLAGS="$flag $PTHREAD_CFLAGS"
        fi

        LIBS="$save_LIBS"
        CFLAGS="$save_CFLAGS"

        # More AIX lossage: must compile with cc_r
        AC_CHECK_PROG(PTHREAD_CC, cc_r, cc_r, ${CC})
else
        PTHREAD_CC="$CC"
fi

AC_SUBST(PTHREAD_LIBS)
AC_SUBST(PTHREAD_CFLAGS)
AC_SUBST(PTHREAD_CC)

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_pthread_ok" = xyes; then
        ifelse([$1],,AC_DEFINE(HAVE_PTHREAD,1,[Define if you have POSIX threads libraries and header files.]),[$1])
        :
else
        acx_pthread_ok=no
        $2
fi

AC_LANG_RESTORE
])dnl ACX_PTHREAD


dnl
dnl Posix Semaphore support
dnl

AC_DEFUN([AM_POSIX_SEM],
[
AC_ARG_ENABLE([posix-sem], AC_HELP_STRING([--disable-posix-sem],
    [do not attempt to use POSIX unnamed semaphores]))

am_posix_sem_ok=no
if test "${enable_posix_sem:-yes}" != "no"; then
    AC_CHECK_HEADERS([semaphore.h], [
	AC_SEARCH_LIBS(sem_init, [posix4 pthread], [
	    AC_MSG_CHECKING([whether to use POSIX unnamed semaphores])
	    AC_RUN_IFELSE([
		AC_LANG_PROGRAM([#include <semaphore.h>], [
		    sem_t mysem;
		    if (sem_init (&mysem, 1, 1) == 0)
		    {
			if (sem_wait (&mysem) == 0)
			{
			    sem_post (&mysem);
			    sem_destroy (&mysem);
			    return 0;
			}
		    }
		    return 1;
		])
		], [
		AC_MSG_RESULT([yes])
		am_posix_sem_ok=yes], [
		AC_MSG_RESULT([no (pshared not usable)])], [
		AC_MSG_RESULT([no (cannot check usability when cross compiling)])])
	])
    ])
fi

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$am_posix_sem_ok" = xyes; then
        ifelse([$1],,AC_DEFINE(HAVE_POSIX_SEMAPHORES),[$1])
        :
else
        am_posix_sem_ok=no
        $2
fi
])


