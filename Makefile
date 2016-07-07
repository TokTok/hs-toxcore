CABAL_VER_NUM := $(shell cabal --numeric-version)
CABAL_VER_MAJOR := $(shell echo $(CABAL_VER_NUM) | cut -f1 -d.)
CABAL_VER_MINOR := $(shell echo $(CABAL_VER_NUM) | cut -f2 -d.)
CABAL_GT_1_22 := $(shell [ $(CABAL_VER_MAJOR) -gt 1 -o \( $(CABAL_VER_MAJOR) -eq 1 -a $(CABAL_VER_MINOR) -ge 22 \) ] && echo true)

ifeq ($(CABAL_GT_1_22),true)
ENABLE_COVERAGE	= --enable-coverage
else
ENABLE_COVERAGE	= --enable-library-coverage
endif

SOURCES	:= $(shell find src test -name "*.*hs" -or -name "*.c" -or -name "*.h")

ifneq ($(wildcard ../tox-spec/pandoc.mk),)
ifneq ($(shell which pandoc),)
DOCS	:= ../tox-spec/spec.md
include ../tox-spec/pandoc.mk
endif
endif

EXTRA_DIRS :=							\
	--extra-include-dirs=$(HOME)/.cabal/extra-dist/include	\
	--extra-lib-dirs=$(HOME)/.cabal/extra-dist/lib

HPC_DIRS = `ls -d dist/hpc/vanilla/mix/* | sed -e 's/^/--hpcdir=/'`


all: check $(DOCS)

check: test-hstox.tix test-toxcore.tix
	hpc sum --exclude=Main --union *.tix --output=dist/hpc/hstox.tix
	hpc markup $(HPC_DIRS) --destdir=dist/hpc/html dist/hpc/hstox.tix > /dev/null
	hpc report $(HPC_DIRS) dist/hpc/hstox.tix

test-%.tix: .build.stamp
	tools/run-tests $*

repl: .build.stamp
	cabal repl

clean:
	cabal clean
	rm -f $(wildcard .*.stamp *.tix)


build: .build.stamp
.build.stamp: $(SOURCES) .configure.stamp .format.stamp .lint.stamp
	rm -f $(wildcard *.tix)
	cabal build
	@touch $@

configure: .configure.stamp
.configure.stamp: .libsodium.stamp
	cabal update
	cabal install --enable-tests $(EXTRA_DIRS) --only-dependencies hstox.cabal
	cabal configure -f asan --enable-tests $(EXTRA_DIRS) $(ENABLE_COVERAGE) --disable-profiling
	@touch $@

doc: $(DOCS)
../tox-spec/spec.md: src/tox/Network/Tox.lhs $(shell find src -name "*.lhs") ../tox-spec/pandoc.mk
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

libsodium: .libsodium.stamp
.libsodium.stamp: tools/install-libsodium
	$<
	@touch $@

format: .format.stamp
.format.stamp: $(SOURCES) .configure.stamp
	if which stylish-haskell; then tools/format-haskell -i src; fi
	@touch $@

lint: .lint.stamp
.lint.stamp: $(SOURCES) .configure.stamp
	if which hlint; then hlint --cross src; fi
	@touch $@
