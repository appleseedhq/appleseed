#!/bin/sh

set -e

# Download dependencies

curl -L "https://github.com/appleseedhq/prebuilt-linux-deps/releases/download/binaries/appleseed-deps-shared-2.0.tgz" > deps.tgz
tar xfz deps.tgz
rm deps.tgz

# Configure cmake

THISDIR=`pwd`
APPLESEED_DEPENDENCIES=$THISDIR/prebuilt-linux-deps

export CMAKE_INCLUDE_PATH=$APPLESEED_DEPENDENCIES/include
export CMAKE_LIBRARY_PATH=$APPLESEED_DEPENDENCIES/lib

export LD_LIBRARY_PATH=$APPLESEED_DEPENDENCIES/lib:../sandbox/lib/Debug:$LD_LIBRARY_PATH

mkdir build
cd build

cmake --version
cmake \
    -D CMAKE_BUILD_TYPE=Debug \
    -D USE_SSE=ON \
    -D USE_SSE42=ON \
    -D HIDE_SYMBOLS=ON \
    -D WITH_CLI=ON \
    -D WITH_STUDIO=ON \
    -D WITH_TOOLS=OFF \
    -D WITH_PYTHON2_BINDINGS=ON \
    -D WITH_DISNEY_MATERIAL=ON \
    -D WITH_EMBREE=ON \
    -D USE_STATIC_BOOST=OFF \
    -D BOOST_INCLUDEDIR=$APPLESEED_DEPENDENCIES/include/boost_1_61_0 \
    -D BOOST_LIBRARYDIR=$APPLESEED_DEPENDENCIES/lib/ \
    -D Boost_ATOMIC_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_atomic-gcc48-mt-1_61.so.1.61.0 \
    -D Boost_CHRONO_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_chrono-gcc48-mt-1_61.so.1.61.0 \
    -D Boost_DATE_TIME_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_date_time-gcc48-mt-1_61.so.1.61.0 \
    -D Boost_FILESYSTEM_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_filesystem-gcc48-mt-1_61.so.1.61.0 \
    -D Boost_PYTHON_LIBRARY=$APPLESEED_DEPENDENCIES/lib/libboost_python-gcc48-mt-1_61.so.1.61.0 \
    -D Boost_PYTHON_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_python-gcc48-mt-1_61.so.1.61.0 \
    -D Boost_REGEX_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_regex-gcc48-mt-1_61.so.1.61.0 \
    -D Boost_SYSTEM_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_system-gcc48-mt-1_61.so.1.61.0 \
    -D Boost_THREAD_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_thread-gcc48-mt-1_61.so.1.61.0 \
    -D Boost_WAVE_LIBRARY_DEBUG=$APPLESEED_DEPENDENCIES/lib/libboost_wave-gcc48-mt-1_61.so.1.61.0 \
    -D EMBREE_INCLUDE_DIR=$APPLESEED_DEPENDENCIES/include \
    -D EMBREE_LIBRARY=$APPLESEED_DEPENDENCIES/lib/libembree3.so \
    -D LZ4_INCLUDE_DIR=$APPLESEED_DEPENDENCIES/include \
    -D LZ4_LIBRARY=$APPLESEED_DEPENDENCIES/lib/liblz4.so \
    -D OPENIMAGEIO_OIIOTOOL=$APPLESEED_DEPENDENCIES/bin/oiiotool \
    -D OSL_COMPILER=$APPLESEED_DEPENDENCIES/bin/oslc \
    -D OSL_MAKETX=$APPLESEED_DEPENDENCIES/bin/maketx \
    -D OSL_QUERY_INFO=$APPLESEED_DEPENDENCIES/bin/oslinfo \
    -D SEEXPREDITOR_INCLUDE_DIR=$APPLESEED_DEPENDENCIES/include \
    -D SEEXPREDITOR_LIBRARY=$APPLESEED_DEPENDENCIES/lib/libSeExprEditor.so \
    ..

make -j 2

echo "Running appleseed tests:"
echo "------------------------"

../sandbox/bin/Debug/appleseed.cli --run-unit-tests --verbose-unit-tests

echo "Running appleseed.python tests:"
echo "-------------------------------"

export PYTHONPATH=$PYTHONPATH:../sandbox/lib/Debug/python
python ../sandbox/lib/Debug/python/appleseed/test/runtests.py

set +e
