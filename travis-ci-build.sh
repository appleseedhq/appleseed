#!/bin/sh

THISDIR=`pwd`

git clone https://github.com/appleseedhq/travis_linux_deps.git
DEPSDIR=$THISDIR/travis_linux_deps

mkdir build
cd build

cmake \
    -DWITH_CLI=ON \
    -DWITH_STUDIO=OFF \
    -DWITH_TOOLS=OFF \
    -DWITH_ALEMBIC=OFF \
    -DWITH_PYTHON=OFF \
    -DBoost_USE_STATIC_LIBS=OFF \
    -DBOOST_INCLUDEDIR=$DEPSDIR/include/boost_1_55_0 \
    -DBOOST_LIBRARYDIR=$DEPSDIR/lib/ \
    -DBoost_CHRONO_LIBRARY_DEBUG=$DEPSDIR/lib/libboost_chrono-gcc48-mt-1_55.so.1.55.0 \
    -DBoost_DATE_TIME_LIBRARY_DEBUG=$DEPSDIR/lib/libboost_datet_ime-gcc48-mt-1_55.so.1.55.0 \
    -DBoost_FILESYSTEM_LIBRARY_DEBUG=$DEPSDIR/lib/libboost_filesystem-gcc48-mt-1_55.so.1.55.0 \
    -DBoost_REGEX_LIBRARY_DEBUG=$DEPSDIR/lib/libboost_regex-gcc48-mt-1_55.so.1.55.0 \
    -DBoost_SYSTEM_LIBRARY_DEBUG=$DEPSDIR/lib/libboost_system-gcc48-mt-1_55.so.1.55.0 \
    -DBoost_THREAD_LIBRARY_DEBUG=$DEPSDIR/lib/libboost_thread-gcc48-mt-1_55.so.1.55.0 \
    -DBoost_WAVE_LIBRARY_DEBUG=$DEPSDIR/lib/libboost_wave-gcc48-mt-1_55.so.1.55.0 \
    -D USE_EXTERNAL_ZLIB=ON \
    -D USE_EXTERNAL_PNG=ON \
    -D USE_EXTERNAL_XERCES=ON \
    -D CMAKE_BUILD_TYPE=Debug \
    ..

make

export LD_LIBRARY_PATH=../sandbox/lib/Debug:$LD_LIBRARY_PATH
../sandbox/bin/Debug/appleseed.cli --run-unit-tests --verbose-unit-tests
