TAG	:= iphydf/haskell-hstox
VERSION	:= 8.0.1

DEPS	:= $(shell perl -n -e 'next LINE if /^\s*--/; $$S == 1 and /:/ and $$S=2; $$S==1 and /, (.*)/ and print " \"$$1\""; not defined $$S and /build-depends:/ and $$S=1;' ../../hstox.cabal)

SCRIPTS := 				\
	user-scripts/install-deps.sh	\
	arm/Dockerfile			\
	x86/Dockerfile			\

build: build-arm build-x86
push: push-arm push-x86
scripts: $(SCRIPTS)

build-%: %/Dockerfile $(wildcard */*) $(SCRIPTS)
	chmod 0644 patches/*
	chmod 0755 *-scripts/*
	docker build -f $< -t $(TAG):$(VERSION)-$* .

push-%: build-%
	docker push $(TAG):$(VERSION)-$*

%/Dockerfile: Dockerfile.in %/.gitignore
	sed -e 's/@GHC_TARGET_ARCH@/$*/' $< > $@

user-scripts/install-deps.sh: user-scripts/install-deps.sh.in ../../hstox.cabal Makefile
	sed -e 's/@DEPENDENCIES@/$(DEPS)/' $< > $@
