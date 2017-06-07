#!/bin/bash

export THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $HOME/ghc-build/set-env.sh
set -x
####################################################################################################

NETWORK_PACKAGE=$NDK/cabal/packages/hackage.haskell.org/network/2.6.3.2/network-2.6.3.2.tar.gz

if [ -f "$NETWORK_PACKAGE" ]; then
  exit 0
fi

$NDK_TARGET-cabal fetch network-2.6.3.2
tar zxf $NETWORK_PACKAGE
pushd network-2.6.3.2
patch -p1 < $SCRIPT_DIR/patches/hs-network-ffi.patch
$NDK_TARGET-cabal install
popd

rm -rf network*
