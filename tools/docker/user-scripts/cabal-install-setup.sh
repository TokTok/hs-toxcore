#!/bin/bash

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $THIS_DIR/set-env.sh
####################################################################################################

${NDK_TARGET}-cabal fetch ${1}-${2}
tar zxf $NDK/cabal/packages/hackage.haskell.org/$1/$2/${1}-${2}.tar.gz

pushd ${1}-${2}
$HOME/.ghc/android-host/bin/cabal configure || true
${NDK_TARGET}-cabal install
popd

rm -rf $1*
