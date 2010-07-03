#!/bin/sh

#~ Copyright 2006-2007 Rene Rivera.
#~ Distributed under the Boost Software License, Version 1.0.
#~ (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt)

#~ Make stage for building.
rm -rf stage
mkdir stage
cd stage

#~ Copy sources to stage.
cd ../src
cp -R *.bat *.jam *.sh *.com *.c *.h *.y *.yy Jambase modules boehm_gc ../stage
find ../stage -name '.svn' -type d -exec rm -rf '{}' ';'
cd ../stage
./build.sh

#~ Build docs, and copy result to stage.
cd ../doc
rm -Rf html
../stage/bin.*/bjam --v2
cp -R html/* ../stage
cd ../stage

#~ Build distribution archives.
sh ./build.sh --- dist
