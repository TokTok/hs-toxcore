#!/bin/bash

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $THIS_DIR/set-env.sh
####################################################################################################

arm-linux-androideabi-cabal fetch ${1}-${2}
tar zxf $NDK/cabal/packages/hackage.haskell.org/$1/$2/${1}-${2}.tar.gz

pushd ${1}-${2}
$HOME/.ghc/android-host/bin/cabal configure || true
arm-linux-androideabi-cabal install
popd

rm -rf $1*
