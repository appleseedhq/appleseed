#!/bin/sh

set -e

THISDIR=`pwd`
mkdir local


echo "Setting up the deps:"
echo "------------------------"
brew install boost --c++11 --without-single --without-static
brew install boost-python
brew install zlib
brew install xerces-c
brew install llvm@3.9

brew install openimageio

brew tap cartr/qt4
brew tap-pin cartr/qt4
brew install qt@4
brew install pyqt@4

mkdir -p $HOME/Library/Python/2.7/lib/python/site-packages
echo 'import site; site.addsitedir("/usr/local/lib/python2.7/site-packages")' >> $HOME/Library/Python/2.7/lib/python/site-packages/homebrew.pth


#OSL
git clone https://github.com/imageworks/OpenShadingLanguage.git
cd OpenShadingLanguage
git checkout Release-1.8.12
mkdir build
cd build
cmake -DLLVM_DIRECTORY=/usr/local/opt/llvm@3.9/ -DLLVM_STATIC=ON -DENABLERTTI=ON -DUSE_LIBCPLUSPLUS=ON -DCMAKE_INSTALL_PREFIX=$THISDIR/local   ..
make install
cd ../..

#SeExpr
git clone https://github.com/wdas/SeExpr
cd SeExpr
git checkout db9610a24401fa7198c54c8768d0484175f54172
mkdir build
cd build
cmake -Wno-dev -DCMAKE_POLICY_DEFAULT_CMP0042=OLD -DCMAKE_INSTALL_PREFIX=$THISDIR/local  ..
make doc install
cd ../..
