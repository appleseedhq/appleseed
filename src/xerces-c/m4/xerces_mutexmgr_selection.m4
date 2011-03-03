dnl @synopsis XERCES_MUTEXMGR_SELECTION
dnl
dnl Determines the which XMLMutexMgr to use
dnl
dnl @category C
dnl @author James Berry
dnl @version 2005-05-25
dnl @license AllPermissive
dnl
dnl $Id: xerces_mutexmgr_selection.m4 467302 2006-10-24 10:45:35Z amassari $

AC_DEFUN([XERCES_MUTEXMGR_SELECTION],
	[
	AC_REQUIRE([XERCES_NO_THREADS])
	AC_REQUIRE([ACX_PTHREAD])
	
	AC_MSG_CHECKING([for which Mutex Manager to use])
	mutexmgr=
	
	# If no threads is specified, use the NoThread Mutex Mgr
	AS_IF([test x$xerces_cv_no_threads = xyes],
		[
			mutexmgr=NoThreads
			AC_DEFINE([XERCES_USE_MUTEXMGR_NOTHREAD], 1, [Define to use the NoThread mutex mgr])
		])
	
	# Platform specific checks
	AS_IF([test -z "$mutexmgr"],
		[
			case $host_os in
			windows* | cygwin* | mingw*)
				mutexmgr=Windows;
				AC_DEFINE([XERCES_USE_MUTEXMGR_WINDOWS], 1, [Define to use the Windows mutex mgr])
				
				case $host_os in
				mingw*)
					CXXFLAGS="${CXXFLAGS} -mthreads"
					;;
				esac
				
				;;
			esac
		])
	
	# Fall back to using posix mutex id we can
	AS_IF([test -z "$mutexmgr" && test x$acx_pthread_ok = xyes],
		[
			mutexmgr=POSIX;
			AC_DEFINE([XERCES_USE_MUTEXMGR_POSIX], 1, [Define to use the POSIX mutex mgr])
			
			# Set additional flags for link and compile
			LIBS="${LIBS} ${PTHREAD_LIBS}"
			CXXFLAGS="${CXXFLAGS} ${PTHREAD_CFLAGS}"
		])
		
	# If we still didn't find a mutex package, bail
	AS_IF([test -z "$mutexmgr"],
		[AC_MSG_ERROR([Xerces cannot function without mutex support. You may want to --disable-threads.])])

	AC_MSG_RESULT($mutexmgr)
	
	# Define the auto-make conditionals which determine what actually gets compiled
	# Note that these macros can't be executed conditionally, which is why they're here, not above.
	AM_CONDITIONAL([XERCES_USE_MUTEXMGR_NOTHREAD],	[test x"$mutexmgr" = xNoThreads])
	AM_CONDITIONAL([XERCES_USE_MUTEXMGR_POSIX],	[test x"$mutexmgr" = xPOSIX])
	AM_CONDITIONAL([XERCES_USE_MUTEXMGR_WINDOWS],	[test x"$mutexmgr" = xWindows])
	
	]
)


