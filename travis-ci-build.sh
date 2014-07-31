#!/bin/sh

rm -f CMakeCache.txt

cmake \
    -D git_tag=1.1.0-travis \
    -D WITH_CLI=ON \
    -D WITH_STUDIO=OFF \
    -D WITH_TOOLS=OFF \
    -D WITH_PYTHON=OFF \
    -D WITH_ALEMBIC=OFF \
    -D WITH_OSL=OFF \
    -D USE_STATIC_BOOST=OFF \
    -D USE_EXTERNAL_ZLIB=ON \
    -D USE_EXTERNAL_EXR=ON \
    -D USE_EXTERNAL_PNG=ON \
    -D USE_EXTERNAL_XERCES=ON \
    -D CMAKE_INSTALL_PREFIX=./travis_install \
    .

make all install package

# Unit tests (appleseed must be installed to function properly)
env LD_LIBRARY_PATH=./travis_install/bin:$LD_LIBRARY_PATH ./travis_install/bin/appleseed.cli --run-unit-tests --verbose-unit-tests
