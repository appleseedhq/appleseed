#!/bin/sh
#
# -D BOOST_ROOT=/sw/external/COS6/include/boost-1_47 \
# -D BOOST_LIBRARYDIR:FILEPATH=/sw/external/COS6/lib64 \
# -D OPENEXR_INCLUDE_DIR=/sw/external/COS6/include \
# -D IMATH_INCLUDE_DIR=/sw/external/COS6/include \
# -D IMATH_HALF_LIBRARY=/sw/external/COS6/lib64/libHalf.so \
# -D IMATH_IEX_LIBRARY=/sw/external/COS6/lib64/libIex.so \
# -D IMATH_MATH_LIBRARY=/sw/external/COS6/lib64/libImath.so \
# -D OPENEXR_IMF_LIBRARY=/sw/external/COS6/lib64/libIlmImf.so \
# -D OPENEXR_THREADS_LIBRARY=/sw/external/COS6/lib64/libIlmThread.so \
# -D QT_QMAKE_EXECUTABLE=/sw/external/COS6/x86_64/qt/current/bin/qmake \
# -D OPENIMAGEIO_LIBRARY=$HOME/systems/oiio/vTrunk/lib/libOpenImageIO.dylib \
# -D OPENIMAGEIO_INCLUDE_DIR=$HOME/systems/oiio/vTrunk/include \
# -D OSL_INCLUDE_DIR=$HOME/systems/osl/v1.5.5/include \
# -D OSL_EXEC_LIBRARY=$HOME/systems/osl/v1.5.5/lib/liboslexec.dylib \
# -D OSL_COMP_LIBRARY=$HOME/systems/osl/v1.5.5/lib/liboslcomp.dylib \
# -D OSL_QUERY_LIBRARY=$HOME/systems/osl/v1.5.5/lib/liboslquery.dylib \
# -D HDF5_USE_STATIC_LIBRARIES=ON \
# -D USE_EXTERNAL_ALEMBIC=ON \
# -D CMAKE_INSTALL_PREFIX=$HOME/systems/appleseed \
#
rm -f CMakeCache.txt
cmake \
    -D git_tag=1.1.0-alpha-20-841-gea2eb00 \
    -D WITH_CLI=OFF \
    -D WITH_STUDIO=FF \
    -D WITH_TOOLS=OFF \
    -D WITH_PYTHON=OFF \
    -D WITH_ALEMBIC=OFF \
    -D WITH_OSL=OFF \
    -D USE_STATIC_BOOST=OFF \
    -D USE_EXTERNAL_ZLIB=ON \
    -D USE_EXTERNAL_EXR=ON \
    -D USE_EXTERNAL_PNG=ON \
    -D USE_EXTERNAL_XERCES=ON \
    .
make all package
