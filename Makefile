COVERAGE = --enable-coverage

SOURCES := $(shell find src -name "*.*hs")

-include ../tox-spec/pandoc.mk

build: dist docs format lint
	cabal build
	ln -sf ../dist/build/test-client/test-client test/test-client
	cabal test

format: .format.stamp
.format.stamp: $(SOURCES)
#	find src -name "*.hs" -exec hindent --style chris-done {} \;
	tools/format-haskell -i src
	@touch $@

lint: .lint.stamp
.lint.stamp: $(SOURCES)
	hlint --cross src
	@touch $@

repl: build
	cabal repl

dist:
	cabal install --only-dependencies --enable-tests
	cabal install stylish-haskell
	cabal install hlint
	cabal configure --enable-tests $(COVERAGE)

clean:
	cabal clean

docs: ../tox-spec/spec.md
../tox-spec/spec.md: src/Network/Tox.lhs $(shell find src -name "*.lhs") Makefile ../tox-spec/pandoc.mk
	echo '% The Tox Reference' > $@
	echo '' >> $@
	pandoc $< $(PANDOC_ARGS)							\
		-f latex+lhs								\
		-t $(FORMAT)								\
		| perl -pe 'BEGIN{undef $$/} s/\`\`\` sourceCode\n.*?\`\`\`\n\n//sg'	\
		>> $@
	pandoc $(PANDOC_ARGS) -f $(FORMAT) -t $(FORMAT) $@ -o $@
