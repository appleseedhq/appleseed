#!/bin/sh

if [ -n "." ] ; then
    if [ "${LD_LIBRARY_PATH+set}" = "set" ] ; then
        export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:."
    else
        export LD_LIBRARY_PATH="."
    fi
fi

exec ./appleseed.studio

