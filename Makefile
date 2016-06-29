CABAL_VER_NUM := $(shell cabal --numeric-version)
CABAL_VER_MAJOR := $(shell echo $(CABAL_VER_NUM) | cut -f1 -d.)
CABAL_VER_MINOR := $(shell echo $(CABAL_VER_NUM) | cut -f2 -d.)
CABAL_GT_1_22 := $(shell [ $(CABAL_VER_MAJOR) -gt 1 -o \( $(CABAL_VER_MAJOR) -eq 1 -a $(CABAL_VER_MINOR) -ge 22 \) ] && echo true)

ifeq ($(CABAL_GT_1_22),true)
COVERAGE = --enable-coverage
else
COVERAGE = --enable-library-coverage
endif

CABAL := cabal --require-sandbox

SOURCES	:= $(shell find src test-server test-tox -name "*.*hs")

ifneq ($(wildcard ../tox-spec/pandoc.mk),)
DOCS	:= ../tox-spec/spec.md
include ../tox-spec/pandoc.mk
endif


all: check $(DOCS)


check: .build.stamp $(wildcard test/*)
	dist/build/test-server/test-server & echo $$! > .server.pid
	$(CABAL) test | grep -v '^Writing: '
	kill `cat .server.pid`
	rm .server.pid

sut-check: .build.stamp
	ln -sf ../dist/build/test-client/test-client test/test-client
	$(CABAL) test | grep -v '^Writing: '
	rm test/test-client

repl: .build.stamp
	$(CABAL) repl

clean:
	$(CABAL) clean
	-test -f .server.pid && kill `cat .server.pid`
	rm -f $(wildcard .*.stamp) .server.pid


build: .build.stamp
.build.stamp: $(SOURCES) .configure.stamp .format.stamp .lint.stamp
	$(CABAL) build
	@touch $@

configure: .configure.stamp
.configure.stamp: .libsodium.stamp .msgpack.stamp
	$(CABAL) configure --enable-tests $(COVERAGE)
	@touch $@

.msgpack.stamp: .sandbox.stamp
	git clone https://github.com/iphydf/msgpack-haskell
	$(CABAL) install					\
		msgpack-haskell/msgpack/msgpack.cabal		\
		msgpack-haskell/msgpack-rpc/msgpack-rpc.cabal
	rm -rf msgpack-haskell
	@touch $@

.sandbox.stamp:
	cabal update
	cabal sandbox init
	cabal --ignore-sandbox install stylish-haskell hlint
	$(CABAL) install --only-dependencies --enable-tests --extra-include-dirs=$(HOME)/.cabal/extra-dist/include --extra-lib-dirs=$(HOME)/.cabal/extra-dist/lib
	@touch $@

doc: $(DOCS)
../tox-spec/spec.md: src/Network/Tox.lhs $(shell find src -name "*.lhs") ../tox-spec/pandoc.mk .pandoc.stamp
	echo '% The Tox Reference' > $@
	echo '' >> $@
	pandoc $< $(PANDOC_ARGS)							\
		-f latex+lhs								\
		-t $(FORMAT)								\
		| perl -pe 'BEGIN{undef $$/} s/\`\`\` sourceCode\n.*?\`\`\`\n\n//sg'	\
		>> $@
	pandoc $(PANDOC_ARGS) -f $(FORMAT) -t $(FORMAT) $@ -o $@
	if which mdl; then $(MAKE) -C ../tox-spec check; fi
	if test -d ../toktok.github.io; then $(MAKE) -C ../toktok.github.io push; fi

pandoc: .pandoc.stamp
.pandoc.stamp:
	cabal --ignore-sandbox install pandoc
	@touch $@

libsodium: .libsodium.stamp
.libsodium.stamp: tools/install-libsodium
	$<
	@touch $@

format: .format.stamp
.format.stamp: $(SOURCES)
	#find src -name "*.hs" -exec hindent --style chris-done {} \;
	tools/format-haskell -i src
	@touch $@

lint: .lint.stamp
.lint.stamp: $(SOURCES)
	hlint --cross src
	@touch $@
