FROM toxchat/toktok-stack:haskell AS build

RUN ["rm", "-rf", "/work/hs-toxcore"]
COPY --chown=builder:users . /work/hs-toxcore
RUN ["stack", "install", "toxcore"]

FROM scratch
COPY --from=build /home/builder/.local/ /
