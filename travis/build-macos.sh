#!/bin/sh

set -e

THISDIR=`pwd`
mkdir local
export LD_LIBRARY_PATH=$THISDIR/local/lib:$APPLESEED_DEPENDENCIES/lib:../sandbox/lib/Debug:$LD_LIBRARY_PATH


echo "Setting up the deps:"
echo "--------------------"
brew upgrade boost
brew install boost-python zlib xerces-c llvm@3.9 openimageio

brew tap cartr/qt4
brew tap-pin cartr/qt4
brew install qt@4 pyqt@4

mkdir -p $HOME/Library/Python/2.7/lib/python/site-packages
echo 'import site; site.addsitedir("/usr/local/lib/python2.7/site-packages")' >> $HOME/Library/Python/2.7/lib/python/site-packages/homebrew.pth


echo "Installing OSL:"
echo "---------------"
#OSL
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
#SeExpr
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
#Main build
mkdir build
cd build
cmake -DWITH_DISNEY_MATERIAL=ON -DUSE_STATIC_BOOST=OFF \
     -DUSE_EXTERNAL_ZLIB=ON -DUSE_EXTERNAL_PNG=ON \
     -DUSE_EXTERNAL_EXR=ON -DUSE_EXTERNAL_XERCES=ON\
     -DUSE_EXTERNAL_SEEXPR=ON -DUSE_EXTERNAL_OIIO=ON \
     -DUSE_EXTERNAL_OCIO=ON -DUSE_EXTERNAL_OSL=ON \
     -DZLIB_INCLUDE_DIR=/usr/local/opt/zlib/include \
     -DZLIB_LIBRARY=/usr/local/opt/zlib/lib/libz.dylib\
     -DPYTHON_INCLUDE_DIR=/usr/local/Cellar/python@2/2.7.15/Frameworks/Python.framework/Versions/2.7/include/python2.7/ \
     -DPYTHON_LIBRARY=/usr/local/Cellar/python@2/2.7.15/Frameworks/Python.framework/Versions/2.7/lib/libpython2.7.dylib \
     -DBoost_PYTHON_LIBRARY_RELEASE=/usr/local/lib/libboost_python27.dylib\
     -DOSL_INCLUDE_DIR=$THISDIR/local/include\
     -DOSL_LIBRARIES=$THISDIR/local/lib\
     -DOSL_EXEC_LIBRARY=$THISDIR/local/lib/liboslexec.dylib \
     -DOSL_COMP_LIBRARY=$THISDIR/local/lib/liboslcomp.dylib\
     -DOSL_QUERY_LIBRARY=$THISDIR/local/lib/liboslq  uery.dylib \
     -DOSL_COMPILER=$THISDIR/local/bin/oslc \
     -DOSL_QUERY_INFO=$THISDIR/local/bin/oslinfo \
     -DSEEXPR_INCLUDE_DIR=$THISDIR/local/include \
     -DSEEXPR_LIBRARY=$THISDIR/local/lib/libSeExpr.dylib \
     -DSEEXPREDITOR_INCLUDE_DIR=$THISDIR/local/include\
     -DSEEXPREDITOR_LIBRARY=$THISDIR/local/lib/libSeExprEditor.dylib\
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
