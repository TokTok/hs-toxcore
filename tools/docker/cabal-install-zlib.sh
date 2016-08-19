#!/bin/bash

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $THIS_DIR/set-env.sh
####################################################################################################

arm-linux-androideabi-cabal fetch zlib-0.6.1.1
tar zxf $NDK/cabal/packages/hackage.haskell.org/zlib/0.6.1.1/zlib-0.6.1.1.tar.gz
pushd zlib-0.6.1.1
patch -p1 < $BASE/patches/hs-zlib-ffi.patch
arm-linux-androideabi-cabal install
popd

rm -rf zlib*
