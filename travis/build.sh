#!/bin/sh

set -e

THISDIR=`pwd`
APPLESEED_DEPENDENCIES=$THISDIR/prebuilt-linux-deps

export CMAKE_INCLUDE_PATH=$APPLESEED_DEPENDENCIES/include
export CMAKE_LIBRARY_PATH=$APPLESEED_DEPENDENCIES/lib

export LD_LIBRARY_PATH=$APPLESEED_DEPENDENCIES/lib:../sandbox/lib/Debug:$LD_LIBRARY_PATH

mkdir build
cd build

cmake --version
cmake \
    -D USE_CPP11=$USE_CPP11 \
    -D WITH_CLI=ON \
    -D WITH_STUDIO=OFF \
    -D WITH_TOOLS=OFF \
    -D WITH_PYTHON=ON \
    -D WITH_DISNEY_MATERIAL=ON \
    -D WITH_NORMALIZED_DIFFUSION_BSSRDF=ON \
    -D PYTHON_INCLUDE_DIR=$APPLESEED_DEPENDENCIES/include/python2.7 \
    -D PYTHON_LIBRARY=$APPLESEED_DEPENDENCIES/lib/libpython2.7.so.1.0 \
    -D Boost_USE_STATIC_LIBS=OFF \
    -D BOOST_INCLUDEDIR=$APPLESEED_DEPENDENCIES/include/boost_1_55_0 \
    -D BOOST_LIBRARYDIR=$APPLESEED_DEPENDENCIES/lib/ \
    -D Boost_CHRONO_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_chrono-gcc48-mt-1_55.so.1.55.0 \
    -D Boost_DATE_TIME_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_date_time-gcc48-mt-1_55.so.1.55.0 \
    -D Boost_FILESYSTEM_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_filesystem-gcc48-mt-1_55.so.1.55.0 \
    -D Boost_PYTHON_LIBRARY=$APPLESEED_DEPENDENCIES/lib/libboost_python-gcc48-mt-1_55.so.1.55.0 \
    -D Boost_PYTHON_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_python-gcc48-mt-1_55.so.1.55.0 \
    -D Boost_REGEX_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_regex-gcc48-mt-1_55.so.1.55.0 \
    -D Boost_SYSTEM_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_system-gcc48-mt-1_55.so.1.55.0 \
    -D Boost_THREAD_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_thread-gcc48-mt-1_55.so.1.55.0 \
    -D Boost_WAVE_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_wave-gcc48-mt-1_55.so.1.55.0 \
    -D USE_EXTERNAL_EXR=ON \
    -D USE_EXTERNAL_SEEXPR=ON \
    -D SEEXPREDITOR_INCLUDE_DIR=$APPLESEED_DEPENDENCIES/include \
    -D SEEXPREDITOR_LIBRARY=$APPLESEED_DEPENDENCIES/lib/libSeExprEditor.so \
    -D USE_EXTERNAL_OIIO=ON \
    -D USE_EXTERNAL_OSL=ON \
    -D OSL_COMPILER=$APPLESEED_DEPENDENCIES/bin/oslc \
    -D OSL_MAKETX=$APPLESEED_DEPENDENCIES/bin/maketx \
    -D OSL_QUERY_INFO=$APPLESEED_DEPENDENCIES/bin/oslinfo \
    -D USE_EXTERNAL_ZLIB=ON \
    -D USE_EXTERNAL_PNG=ON \
    -D USE_EXTERNAL_XERCES=ON \
    -D USE_SSE=ON \
    -D USE_SSE42=ON \
    -D HIDE_SYMBOLS=ON \
    -D CMAKE_BUILD_TYPE=Debug \
    ..

make -j 2

echo "Running appleseed tests:"
echo "------------------------"

../sandbox/bin/Debug/appleseed.cli --run-unit-tests --verbose-unit-tests

echo "Running appleseed.python tests:"
echo "-------------------------------"

export PYTHONPATH=$PYTHONPATH:../sandbox/lib/Debug/python2.7
python ../sandbox/lib/Debug/python2.7/appleseed/test/runtests.py

set +e
