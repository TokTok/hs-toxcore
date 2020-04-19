#!/bin/bash

source "$HOME/ghc-build/set-env-android.sh"; cd -
####################################################################################################

rm -rf "$@"
"$NDK_TARGET-cabal" get "$@"
sed -i -e 's/build-type:.*Custom/build-type: Simple/' -- */*.cabal

cat <<EOF > cabal.project
packages: *.cabal, */*.cabal
EOF
