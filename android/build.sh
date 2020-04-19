#!/bin/bash

# TODO(iphydf): Undo this once ghc-android Docker images are updated.
# shellcheck disable=SC2016
sed -i -e 's@^\. set-env.sh@. $HOME/ghc-build/set-env.sh@' "$HOME/ghc-build/set-env-android.sh"

export THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
. "$HOME/ghc-build/set-env-android.sh"; cd -
set -x

"$NDK_TARGET-cabal" update

"$THIS_DIR/user-scripts/install-libsodium.sh"
"$THIS_DIR/user-scripts/cabal-prepare.sh" comonad-5.0.6 distributive-0.6.1 entropy-0.4.1.5 network-2.8.0.1

(cd network-2.8.0.1 && patch -p1 < ../android/patches/hs-network-ffi.patch)

"$NDK_TARGET-cabal" new-install -j4 --with-happy="$(which happy)"
