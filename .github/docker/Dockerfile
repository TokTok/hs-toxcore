# hadolint ignore=DL3007
FROM toxchat/haskell:latest

RUN ["rm", "-rf", "/work/hs-toxcore"]
COPY --chown=builder:users . /work/hs-toxcore
RUN ["stack", "build", "toxcore"]
