# vim:ft=dockerfile
FROM iphydf/haskell:8.0.1 # https://github.com/iphydf/docker-build-ghc-android/blob/master/Dockerfile
MAINTAINER iphydf@gmail.com

WORKDIR $BASE

COPY build-libsodium.sh $BASE/
RUN ./build-libsodium.sh

#
# Install prerequisite haskell packages.
#
COPY *.patch $BASE/patches/
COPY cabal-install-zlib.sh $BASE/
RUN ./cabal-install-zlib.sh

COPY cabal-install-setup.sh $BASE/
RUN ./cabal-install-setup.sh distributive 0.5.0.2 \
 && ./cabal-install-setup.sh comonad 5

RUN arm-linux-androideabi-cabal install \
 base16-bytestring \
 binary-bits \
 binary-conduit \
 conduit-extra \
 data-binary-ieee754 \
 iproute \
 network-2.6.2.1 \
 QuickCheck-2.9.1 \
 saltine \
 split \
 unordered-containers

WORKDIR $HOME

RUN git clone --depth=1 --branch=docker https://github.com/iphydf/hstox \
 && arm-linux-androideabi-cabal install hstox/hstox.cabal -flibrary-only \
 --enable-tests \
 --enable-benchmarks \
 --only-dependencies \
 && rm -rf hstox

RUN sed -i -e 's|#/!|#!/|' /home/androidbuilder/.ghc/android-14/arm-linux-androideabi-4.8/bin/arm-linux-androideabi-cabal

USER root
RUN apt-get install -y runit

ENTRYPOINT ["/home/androidbuilder/entrypoint.sh"]
WORKDIR /work
COPY entrypoint.sh /home/androidbuilder/
