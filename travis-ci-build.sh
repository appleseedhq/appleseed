#!/bin/sh

git clone https://github.com/appleseedhq/travis_linux_deps.git

mkdir build
cd build

cmake \
    -D WITH_CLI=ON \
    -D WITH_STUDIO=OFF \
    -D WITH_TOOLS=OFF \
    -D WITH_PYTHON=ON \
    -D WITH_ALEMBIC=OFF \
    -D WITH_OSL=OFF \
    -D USE_STATIC_BOOST=OFF \
    -D USE_EXTERNAL_ZLIB=ON \
    -D USE_EXTERNAL_EXR=ON \
    -D USE_EXTERNAL_PNG=ON \
    -D USE_EXTERNAL_XERCES=ON \
    -D CMAKE_BUILD_TYPE=Debug \
    ..

make

export LD_LIBRARY_PATH=../sandbox/lib/Debug:$LD_LIBRARY_PATH
../sandbox/bin/Debug/appleseed.cli --run-unit-tests --verbose-unit-tests
