#!/bin/sh
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
