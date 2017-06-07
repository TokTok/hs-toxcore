#!/bin/bash

export THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $HOME/ghc-build/set-env.sh
set -x
####################################################################################################

if [ -f "$PLATFORM_PREFIX/.cabal/bin/hspec-discover" ]; then
  exit 0
fi

# See install-happy.sh for explanation for why we do this.

cabal install hspec-discover
$THIS_DIR/cabal-install-setup.sh hspec-discover 2.4.3
cp $HOME/.cabal/bin/hspec-discover $PLATFORM_PREFIX/.cabal/bin/
