#!/bin/bash

export THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
. "$HOME/ghc-build/set-env-android.sh"
cd - || exit
set -x

export PKG_CONFIG_PATH="$NDK_ADDON_PREFIX/lib/pkgconfig"

"$NDK_TARGET-cabal" update

"$THIS_DIR/user-scripts/install-libsodium.sh"
"$THIS_DIR/user-scripts/cabal-prepare.sh" comonad-5.0.8 distributive-0.6.2.1 entropy-0.4.1.6 network-3.1.1.1

(cd network-3.1.1.1 && patch -p1 <../android/patches/hs-network-ffi.patch)

"$NDK_TARGET-cabal" new-install -j4 --with-happy="$GHC_HOST/bin/happy" --ghc-option="-fPIC" -f-webservice
