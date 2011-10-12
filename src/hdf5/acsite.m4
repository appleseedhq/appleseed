dnl -------------------------------------------------------------------------
dnl -------------------------------------------------------------------------
dnl
dnl Copyright by the Board of Trustees of the University of Illinois.
dnl All rights reserved.
dnl
dnl This file is part of HDF5.  The full HDF5 copyright notice, including
dnl terms governing use, modification, and redistribution, is contained in
dnl the files COPYING and Copyright.html.  COPYING can be found at the root
dnl of the source code distribution tree; Copyright.html can be found at the
dnl root level of an installed copy of the electronic HDF5 document set and
dnl is linked from the top-level documents page.  It can also be found at
dnl http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have
dnl access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu.
dnl
dnl Macros for HDF5 Fortran
dnl
dnl -------------------------------------------------------------------------
dnl -------------------------------------------------------------------------

dnl -------------------------------------------------------------------------
dnl _AC_SYS_LARGEFILE_MACRO_VALUE
dnl
dnl The following macro overrides the autoconf macro of the same name
dnl with this custom definition. This macro performs the same checks as 
dnl autoconf's native _AC_SYS_LARGEFILE_MACRO_VALUE, but will also set
dnl AM_CPPFLAGS with the appropriate -D defines so additional configure 
dnl sizeof checks do not fail.
dnl
# _AC_SYS_LARGEFILE_MACRO_VALUE(C-MACRO, VALUE,
#               CACHE-VAR,
#               DESCRIPTION,
#               PROLOGUE, [FUNCTION-BODY])
# ----------------------------------------------------------
m4_define([_AC_SYS_LARGEFILE_MACRO_VALUE],
[AC_CACHE_CHECK([for $1 value needed for large files], [$3],
[while :; do
  m4_ifval([$6], [AC_LINK_IFELSE], [AC_COMPILE_IFELSE])(
    [AC_LANG_PROGRAM([$5], [$6])],
    [$3=no; break])
  m4_ifval([$6], [AC_LINK_IFELSE], [AC_COMPILE_IFELSE])(
    [AC_LANG_PROGRAM([@%:@define $1 $2
$5], [$6])],
    [$3=$2; break])
  $3=unknown
  break
done])
case $$3 in #(
  no | unknown) ;;
  *) AC_DEFINE_UNQUOTED([$1], [$$3], [$4])
     AM_CPPFLAGS="-D$1=$$3 $AM_CPPFLAGS";;
esac
rm -rf conftest*[]dnl
])# _AC_SYS_LARGEFILE_MACRO_VALUE

dnl -------------------------------------------------------------------------
dnl AC_F9X_MODS()
dnl
dnl	Check how F9X handles modules. This macro also checks which
dnl	command-line option to use to include the module once it's built.
dnl
AC_DEFUN(AC_F9X_MODS,
[AC_MSG_CHECKING(what $FC does with modules)
AC_LANG_PUSH(Fortran)

test -d conftestdir || mkdir conftestdir
cd conftestdir
rm -rf *

cat >conftest.$ac_ext <<EOF
      module module
         integer foo
      end module module
EOF

eval $ac_compile
modfiles=""
F9XMODEXT=""

for f in conftest.o module.mod MODULE.mod module.M MODULE.M; do
  if test -f "$f" ; then
    modfiles="$f"

    case "$f" in
      *.o)   F9XMODEXT="o" ;;
      *.mod) F9XMODEXT="mod" ;;
      *.M)   F9XMODEXT="M" ;;
    esac
  fi
done

echo $modfiles 6>&1
if test "$modfiles" = file.o; then
  echo $ac_n "checking whether $FC -em is saner""... $ac_c" 1>&6
  OLD_FCFLAGS=$FCFLAGS
  FCFLAGS="$FCFLAGS -em"
  eval $ac_compile
  modfiles=""
  for f in file.o module.mod MODULE.mod module.M MODULE.M; do
    test -f $f && modfiles="$f"
  done
  if test "$modfiles" = "file.o"; then
    FCFLAGS=$OLD_FCFLAGS
    echo no 6>&1
  else
    echo yes 6>&1
  fi
fi
cd ..

AC_MSG_CHECKING(how $FC finds modules)

for flag in "-I" "-M" "-p"; do
  cat >conftest.$ac_ext <<EOF
      program conftest
          use module
      end program conftest
EOF

  ac_compile='${FC-f90} $FCFLAGS ${flag}conftestdir -c conftest.$ac_ext 1>&AS_MESSAGE_LOG_FD'

  if AC_TRY_EVAL(ac_compile); then
    F9XMODFLAG=$flag
    break
  fi
done

if test -n "$F9XMODFLAG"; then
  echo $F9XMODFLAG 1>&6
  FCFLAGS="$F9XMODFLAG. $FCFLAGS"
else
  echo unknown 1>&6
fi
AC_SUBST(F9XMODFLAG)
AC_SUBST(F9XMODEXT)
rm -rf conftest*
AC_LANG_POP(Fortran)
])

