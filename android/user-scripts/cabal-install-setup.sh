#!/bin/bash

export THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $HOME/ghc-build/set-env.sh
set -x
####################################################################################################

PACKAGE=$NDK/cabal/packages/hackage.haskell.org/$1/$2/$1-$2.tar.gz

if [ -f "$PACKAGE" ]; then
  exit 0
fi

$NDK_TARGET-cabal fetch $1-$2
tar zxf $PACKAGE

pushd $1-$2
$HOME/.ghc/android-host/bin/cabal configure || true
$NDK_TARGET-cabal install
popd

rm -rf $1*
