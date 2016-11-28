#!/bin/sh

set -e

THISDIR=`pwd`
DEPSDIR=$THISDIR/travis-linux-deps

export LD_LIBRARY_PATH=$DEPSDIR/lib:../sandbox/lib/Debug:$LD_LIBRARY_PATH

mkdir build
cd build

cmake \
    -D USE_CPP11=$USE_CPP11 \
    -D WITH_CLI=ON \
    -D WITH_STUDIO=OFF \
    -D WITH_TOOLS=OFF \
    -D WITH_ALEMBIC=OFF \
    -D WITH_PYTHON=ON \
    -D WITH_DISNEY_MATERIAL=ON \
    -D WITH_OSL=ON \
    -D WITH_NORMALIZED_DIFFUSION_BSSRDF=ON \
    -D Boost_USE_STATIC_LIBS=OFF \
    -D BOOST_INCLUDEDIR=$DEPSDIR/include/boost_1_55_0 \
    -D BOOST_LIBRARYDIR=$DEPSDIR/lib/ \
    -D Boost_CHRONO_LIBRARY_DEBUG=$DEPSDIR/lib/libboost_chrono-gcc48-mt-1_55.so.1.55.0 \
    -D Boost_DATE_TIME_LIBRARY_DEBUG=$DEPSDIR/lib/libboost_date_time-gcc48-mt-1_55.so.1.55.0 \
    -D Boost_FILESYSTEM_LIBRARY_DEBUG=$DEPSDIR/lib/libboost_filesystem-gcc48-mt-1_55.so.1.55.0 \
    -D Boost_PYTHON_LIBRARY=$DEPSDIR/lib/libboost_python-gcc48-mt-1_55.so.1.55.0 \
    -D Boost_PYTHON_LIBRARY_DEBUG=$DEPSDIR/lib/libboost_python-gcc48-mt-1_55.so.1.55.0 \
    -D Boost_REGEX_LIBRARY_DEBUG=$DEPSDIR/lib/libboost_regex-gcc48-mt-1_55.so.1.55.0 \
    -D Boost_SYSTEM_LIBRARY_DEBUG=$DEPSDIR/lib/libboost_system-gcc48-mt-1_55.so.1.55.0 \
    -D Boost_THREAD_LIBRARY_DEBUG=$DEPSDIR/lib/libboost_thread-gcc48-mt-1_55.so.1.55.0 \
    -D Boost_WAVE_LIBRARY_DEBUG=$DEPSDIR/lib/libboost_wave-gcc48-mt-1_55.so.1.55.0 \
    -D USE_EXTERNAL_EXR=ON \
    -D IMATH_INCLUDE_DIR=$DEPSDIR/include \
    -D IMATH_HALF_LIBRARY=$DEPSDIR/lib/libHalf.so.12 \
    -D IMATH_IEX_LIBRARY=$DEPSDIR/lib/libIex-2_2.so.12 \
    -D IMATH_MATH_LIBRARY=$DEPSDIR/lib/libImath-2_2.so.12 \
    -D OPENEXR_INCLUDE_DIR=$DEPSDIR/include \
    -D OPENEXR_THREADS_LIBRARY=$DEPSDIR/lib/libIlmThread-2_2.so.12 \
    -D OPENEXR_IMF_LIBRARY=$DEPSDIR/lib/libIlmImf-2_2.so.22 \
    -D USE_EXTERNAL_SEEXPR=ON \
    -D SEEXPR_INCLUDE_DIR=$DEPSDIR/include \
    -D SEEXPR_LIBRARY=$DEPSDIR/lib/libSeExpr.so \
    -D USE_EXTERNAL_OIIO=ON \
    -D OPENIMAGEIO_INCLUDE_DIR=$DEPSDIR/include \
    -D OPENIMAGEIO_LIBRARY=$DEPSDIR/lib/libOpenImageIO.so.1.7 \
    -D USE_EXTERNAL_OSL=ON \
    -D OSL_INCLUDE_DIR=$DEPSDIR/include \
    -D OSL_COMP_LIBRARY=$DEPSDIR/lib/liboslcomp.so \
    -D OSL_EXEC_LIBRARY=$DEPSDIR/lib/liboslexec.so \
    -D OSL_QUERY_LIBRARY=$DEPSDIR/lib/liboslquery.so \
    -D OSL_COMPILER=$DEPSDIR/bin/oslc \
    -D OSL_MAKETX=$DEPSDIR/bin/maketx \
    -D OSL_QUERY_INFO=$DEPSDIR/bin/oslinfo \
    -D PYTHON_INCLUDE_DIR=$DEPSDIR/include/python2.7 \
    -D PYTHON_LIBRARY=$DEPSDIR/lib/libpython2.7.so.1.0 \
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
