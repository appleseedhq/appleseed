#!/bin/sh

#~ Copyright 2006-2008 Rene Rivera.
#~ Distributed under the Boost Software License, Version 1.0.
#~ (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt)

#~ BJAM=bjam
#~ BJAM_SRC=../src
#~ BJAM_BIN=`ls -1 ${BJAM_SRC}/bin.*/bjam`

# Run a command, and echo before doing so. Also checks the exit
# status and quits if there was an error.
echo_run ()
{
    echo "$@"
    $@
    r=$?
    if test $r -ne 0 ; then
        exit $r
    fi
}

# Check that a command is in the PATH.
test_path ()
{
    if `command -v command 1>/dev/null 2>/dev/null`; then
        command -v $1 1>/dev/null 2>/dev/null
    else
        hash $1 1>/dev/null 2>/dev/null
    fi
}

Guess_BJAM ()
{
    if test_path bjam ; then BJAM=bjam
    elif test -r ./bjam ; then BJAM=./bjam
    elif test -r "${BJAM_BIN}" ; then BJAM="${BJAM_BIN}"
    fi
}

Build_BJAM_To_Test ()
{
    cwd=`pwd`
    if test "${BJAM_SRC}" = "" ; then BJAM_SRC=../src ; fi
    cd "${BJAM_SRC}"
    ./build.sh
    if test "${BJAM_BIN}" = "" ; then BJAM_BIN=`ls -1 ${BJAM_SRC}/bin.*/bjam` ; fi
    cd "${cwd}"
}

Build_BJAM_To_Test
Guess_BJAM
echo_run "${BJAM}" -f test.jam "-sBJAM=${BJAM_BIN}" "$@"
