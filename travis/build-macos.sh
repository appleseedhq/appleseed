#!/bin/sh

set -e

THISDIR=`pwd`
mkdir local

export LD_LIBRARY_PATH=$THISDIR/local/lib:$APPLESEED_DEPENDENCIES/lib:../sandbox/lib/Debug:$LD_LIBRARY_PATH
export DYLD_LIBRARY_PATH=$THISDIR/local/lib:$DYLD_LIBRARY_PATH

echo "Setting up the deps:"
echo "--------------------"

echo "============= Updating the formulas"
brew update
brew upgrade

echo "============= Installing deps"
brew install boost-python llvm@3.9 lz4 openimageio xerces-c zlib
brew tap cartr/qt4
brew tap-pin cartr/qt4
brew install qt@4 pyqt@4

mkdir -p $HOME/Library/Python/2.7/lib/python/site-packages
echo 'import site; site.addsitedir("/usr/local/lib/python2.7/site-packages")' >> $HOME/Library/Python/2.7/lib/python/site-packages/homebrew.pth

echo "Installing OSL:"
echo "---------------"
git clone https://github.com/imageworks/OpenShadingLanguage.git
cd OpenShadingLanguage
git checkout Release-1.8.12
mkdir build
cd build
cmake -DLLVM_DIRECTORY=/usr/local/opt/llvm@3.9/ -DLLVM_STATIC=ON -DENABLERTTI=ON -DUSE_LIBCPLUSPLUS=ON -DCMAKE_INSTALL_PREFIX=$THISDIR/local   ..
make install -j 2
cd ../..

echo "Installing SeExpr:"
echo "------------------"
git clone https://github.com/wdas/SeExpr
cd SeExpr
git checkout db9610a24401fa7198c54c8768d0484175f54172
mkdir build
cd build
cmake -Wno-dev -DCMAKE_POLICY_DEFAULT_CMP0042=OLD -DCMAKE_INSTALL_PREFIX=$THISDIR/local  ..
mkdir src/doc/html
make install -j 2
cd ../..

echo "Main build:"
echo "-----------"
mkdir build
cd build
cmake \
    -D CMAKE_BUILD_TYPE=Debug \
    -D HIDE_SYMBOLS=ON \
    -D WITH_STUDIO=OFF \
    -D WITH_PYTHON2_BINDINGS=OFF \
    -D WITH_DISNEY_MATERIAL=ON \
    -D WITH_EMBREE=OFF \
    -D USE_STATIC_BOOST=OFF \
    -D Boost_PYTHON_LIBRARY_RELEASE=/usr/local/lib/libboost_python27.dylib \
    -D OSL_INCLUDE_DIR=$THISDIR/local/include \
    -D OSL_LIBRARIES=$THISDIR/local/lib \
    -D OSL_EXEC_LIBRARY=$THISDIR/local/lib/liboslexec.dylib \
    -D OSL_COMP_LIBRARY=$THISDIR/local/lib/liboslcomp.dylib \
    -D OSL_QUERY_LIBRARY=$THISDIR/local/lib/liboslquery.dylib \
    -D OSL_COMPILER=$THISDIR/local/bin/oslc \
    -D OSL_QUERY_INFO=$THISDIR/local/bin/oslinfo \
    -D PYTHON_INCLUDE_DIR=/usr/local/Cellar/python@2/2.7.15/Frameworks/Python.framework/Versions/2.7/include/python2.7/ \
    -D PYTHON_LIBRARY=/usr/local/Cellar/python@2/2.7.15/Frameworks/Python.framework/Versions/2.7/lib/libpython2.7.dylib \
    -D SEEXPR_INCLUDE_DIR=$THISDIR/local/include \
    -D SEEXPR_LIBRARY=$THISDIR/local/lib/libSeExpr.dylib \
    -D SEEXPREDITOR_INCLUDE_DIR=$THISDIR/local/include \
    -D SEEXPREDITOR_LIBRARY=$THISDIR/local/lib/libSeExprEditor.dylib \
    -D ZLIB_INCLUDE_DIR=/usr/local/opt/zlib/include \
    -D ZLIB_LIBRARY=/usr/local/opt/zlib/lib/libz.dylib \
    ..
make -j 2

echo "Running appleseed tests:"
echo "------------------------"
../sandbox/bin/Debug/appleseed.cli --run-unit-tests --verbose-unit-tests

#echo "Running appleseed.python tests:"
#echo "-------------------------------"
#export PYTHONPATH=$PYTHONPATH:../sandbox/lib/Debug/python
#python ../sandbox/lib/Debug/python/appleseed/test/runtests.py

set +e
