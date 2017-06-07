#!/bin/bash

export THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $HOME/ghc-build/set-env.sh
set -x
####################################################################################################

ZLIB_PACKAGE=$NDK/cabal/packages/hackage.haskell.org/zlib/0.6.1.1/zlib-0.6.1.1.tar.gz

if [ -f "$ZLIB_PACKAGE" ]; then
  exit 0
fi

$NDK_TARGET-cabal fetch zlib-0.6.1.1
tar zxf $ZLIB_PACKAGE
pushd zlib-0.6.1.1
patch -p1 < $SCRIPT_DIR/patches/hs-zlib-ffi.patch
$NDK_TARGET-cabal install
popd

rm -rf zlib*
