#!/bin/bash

source "$HOME/ghc-build/set-env-android.sh"
####################################################################################################

if [ -f "$NDK_ADDON_PREFIX/lib/libsodium.a" ]; then
  exit 0
fi

cd "$NDK_ADDON_SRC"
apt-get source libsodium

pushd libsodium*/
autoreconf -i
./configure \
  --prefix="$NDK_ADDON_PREFIX" \
  --host="$NDK_TARGET" \
  --build="$BUILD_ARCH" \
  --with-build-cc="$BUILD_GCC" \
  --enable-static \
  --disable-shared
make $MAKEFLAGS
make install
popd

rm -rf libsodium*
