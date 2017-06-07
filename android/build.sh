#!/bin/bash

set -exu

export SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

$NDK_TARGET-cabal update

$SCRIPT_DIR/user-scripts/cabal-install-clock.sh
$SCRIPT_DIR/user-scripts/cabal-install-network.sh
$SCRIPT_DIR/user-scripts/cabal-install-setup.sh distributive 0.5.0.2
$SCRIPT_DIR/user-scripts/cabal-install-setup.sh comonad 5
$SCRIPT_DIR/user-scripts/cabal-install-setup.sh entropy 0.3.7
$SCRIPT_DIR/user-scripts/cabal-install-zlib.sh
$SCRIPT_DIR/user-scripts/install-happy.sh
$SCRIPT_DIR/user-scripts/install-hspec.sh
$SCRIPT_DIR/user-scripts/install-libsodium.sh

$NDK_TARGET-cabal install --enable-tests --enable-benchmarks --only-dependencies
$NDK_TARGET-cabal install -flibrary-only
