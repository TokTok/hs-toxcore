#!/bin/bash

export THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $HOME/ghc-build/set-env.sh
set -x
####################################################################################################

CLOCK_PACKAGE=$NDK/cabal/packages/hackage.haskell.org/clock/0.7.2/clock-0.7.2.tar.gz

if [ -f "$CLOCK_PACKAGE" ]; then
  exit 0
fi

$NDK_TARGET-cabal fetch clock-0.7.2
tar zxf $CLOCK_PACKAGE
pushd clock-0.7.2
patch -p1 < $SCRIPT_DIR/patches/hs-clock-ffi.patch
$NDK_TARGET-cabal install
popd

rm -rf clock*
