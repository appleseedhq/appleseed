#!/bin/bash

#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2014-2019 Nicholas Yue, The appleseedhq Organization
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#


set -e

THISDIR=`pwd`


#--------------------------------------------------------------------------------------------------
# Download and unpack dependencies.
#--------------------------------------------------------------------------------------------------

echo "travis_fold:start:deps"
echo "Downloading and unpacking dependencies..."

curl -L "https://github.com/appleseedhq/prebuilt-linux-deps/releases/download/binaries/appleseed-deps-shared-2.0.tgz" > deps.tgz
tar xfz deps.tgz
rm deps.tgz

echo "travis_fold:end:deps"


#--------------------------------------------------------------------------------------------------
# Configure CMake.
#--------------------------------------------------------------------------------------------------

cmake --version

APPLESEED_DEPENDENCIES=$THISDIR/prebuilt-linux-deps

export CMAKE_INCLUDE_PATH=$APPLESEED_DEPENDENCIES/include
export CMAKE_LIBRARY_PATH=$APPLESEED_DEPENDENCIES/lib


#--------------------------------------------------------------------------------------------------
# Prepare to run appleseed.
# This must be done before compiling appleseed because the compiling process needs to invokes oslc.
#--------------------------------------------------------------------------------------------------

export LD_LIBRARY_PATH=$APPLESEED_DEPENDENCIES/lib:sandbox/lib/Debug:$LD_LIBRARY_PATH
export PYTHONPATH=$PYTHONPATH:sandbox/lib/Debug/python


#--------------------------------------------------------------------------------------------------
# Build appleseed.
#--------------------------------------------------------------------------------------------------

echo "travis_fold:start:build"
echo "Building appleseed..."

mkdir build
pushd build

# TODO: is it necessary to set DBoost_PYTHON_LIBRARY?
cmake \
    -DCMAKE_BUILD_TYPE=Debug \
    -DUSE_SSE42=ON \
    -DWITH_DISNEY_MATERIAL=ON \
    -DWITH_EMBREE=ON \
    -DUSE_STATIC_BOOST=OFF \
    -DBOOST_INCLUDEDIR=$APPLESEED_DEPENDENCIES/include/boost_1_61_0 \
    -DBOOST_LIBRARYDIR=$APPLESEED_DEPENDENCIES/lib/ \
    -DBoost_ATOMIC_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_atomic-gcc48-mt-1_61.so.1.61.0 \
    -DBoost_CHRONO_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_chrono-gcc48-mt-1_61.so.1.61.0 \
    -DBoost_DATE_TIME_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_date_time-gcc48-mt-1_61.so.1.61.0 \
    -DBoost_FILESYSTEM_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_filesystem-gcc48-mt-1_61.so.1.61.0 \
    -DBoost_PYTHON_LIBRARY=$APPLESEED_DEPENDENCIES/lib/libboost_python-gcc48-mt-1_61.so.1.61.0 \
    -DBoost_PYTHON_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_python-gcc48-mt-1_61.so.1.61.0 \
    -DBoost_REGEX_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_regex-gcc48-mt-1_61.so.1.61.0 \
    -DBoost_SYSTEM_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_system-gcc48-mt-1_61.so.1.61.0 \
    -DBoost_THREAD_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_thread-gcc48-mt-1_61.so.1.61.0 \
    -DBoost_WAVE_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_wave-gcc48-mt-1_61.so.1.61.0 \
    -DEMBREE_INCLUDE_DIR=$APPLESEED_DEPENDENCIES/include \
    -DEMBREE_LIBRARY=$APPLESEED_DEPENDENCIES/lib/libembree3.so \
    -DLZ4_INCLUDE_DIR=$APPLESEED_DEPENDENCIES/include \
    -DLZ4_LIBRARY=$APPLESEED_DEPENDENCIES/lib/liblz4.so \
    -DOPENIMAGEIO_OIIOTOOL=$APPLESEED_DEPENDENCIES/bin/oiiotool \
    -DOPENIMAGEIO_IDIFF=$APPLESEED_DEPENDENCIES/bin/idiff \
    -DOSL_COMPILER=$APPLESEED_DEPENDENCIES/bin/oslc \
    -DOSL_MAKETX=$APPLESEED_DEPENDENCIES/bin/maketx \
    -DOSL_QUERY_INFO=$APPLESEED_DEPENDENCIES/bin/oslinfo \
    -DSEEXPREDITOR_INCLUDE_DIR=$APPLESEED_DEPENDENCIES/include \
    -DSEEXPREDITOR_LIBRARY=$APPLESEED_DEPENDENCIES/lib/libSeExprEditor.so \
    ..

make -j 2

popd

echo "travis_fold:end:build"


#--------------------------------------------------------------------------------------------------
# Run appleseed unit tests.
#--------------------------------------------------------------------------------------------------

echo "travis_fold:start:unit-tests"
echo "Running appleseed unit tests..."

sandbox/bin/Debug/appleseed.cli --run-unit-tests --verbose-unit-tests

echo "travis_fold:end:unit-tests"


#--------------------------------------------------------------------------------------------------
# Run appleseed.python unit tests.
#--------------------------------------------------------------------------------------------------

echo "travis_fold:start:python-unit-tests"
echo "Running appleseed.python unit tests..."

python sandbox/lib/Debug/python/appleseed/test/runtests.py

echo "travis_fold:end:python-unit-tests"


set +e
