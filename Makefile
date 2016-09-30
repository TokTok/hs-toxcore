include configure.mk

SRCDIRS	:= src test tools
SOURCES	:= $(shell find $(SRCDIRS) -name "*.*hs" -or -name "*.c" -or -name "*.h")

export LD_LIBRARY_PATH := $(HOME)/.cabal/extra-dist/lib

EXTRA_DIRS :=							\
	--extra-include-dirs=$(HOME)/.cabal/extra-dist/include	\
	--extra-lib-dirs=$(HOME)/.cabal/extra-dist/lib

CONFIGURE_FLAGS :=		\
	--enable-benchmarks	\
	--enable-tests		\
	$(DISABLE_PROFILING)	\
	$(EXTRA_DIRS)

HSTOX_TIX	= dist/hpc/tix/hstox/hstox.tix


all: check $(DOCS)

check: $(HSTOX_TIX)
	hpc markup $(HPC_DIRS) --destdir=dist/hpc/html $< > /dev/null
	hpc report $(HPC_DIRS) $<

$(HSTOX_TIX): .build.stamp
	cabal test $(CABAL_JOBS) | grep -v '^Writing: '
	mkdir -p $(@D)
	hpc sum --exclude=Main --union `find dist -name "*.tix" -and -not -wholename "*$@"` --output=$@

repl:
	rm -f .configure.stamp
	cabal configure $(CONFIGURE_FLAGS)
	cabal repl

clean:
	cabal clean
	rm -f $(wildcard .*.stamp *.tix)


build: .build.stamp
.build.stamp: $(SOURCES) .configure.stamp .format.stamp .lint.stamp
	rm -f $(wildcard *.tix)
	cabal build $(CABAL_JOBS)
	@touch $@

configure: .configure.stamp
.configure.stamp: .libsodium.stamp hstox.cabal
	cabal update
	cabal install $(CONFIGURE_FLAGS) --only-dependencies $(CABAL_JOBS)
	cabal configure $(CONFIGURE_FLAGS) $(ENABLE_COVERAGE)
	@touch $@

doc: $(DOCS)
../spec/spec.md: src/tox/Network/Tox.lhs $(shell find src -name "*.lhs") ../spec/pandoc.mk Makefile
	echo '% The Tox Reference' > $@
	echo '' >> $@
	pandoc $< $(PANDOC_ARGS)							\
		-f latex+lhs								\
		-t $(FORMAT)								\
		| perl -pe 'BEGIN{undef $$/} s/\`\`\` sourceCode\n.*?\`\`\`\n\n//sg'	\
		>> $@
	pandoc $(PANDOC_ARGS) -f $(FORMAT) -t $(FORMAT) $@ -o $@
	if which mdl; then $(MAKE) -C $(@D) check; fi
	cd $(@D) && git diff

libsodium: .libsodium.stamp
.libsodium.stamp: tools/install-libsodium
	$<
	@touch $@

format: .format.stamp
.format.stamp: $(SOURCES) .configure.stamp
	if which stylish-haskell; then tools/format-haskell -i $(SRCDIRS); fi
	@touch $@

lint: .lint.stamp
.lint.stamp: $(SOURCES) .configure.stamp
	if which hlint; then hlint --cross src; fi
	@touch $@
