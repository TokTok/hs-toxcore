TAG	:= iphydf/haskell-hstox
VERSION	:= 8.0.1

SCRIPTS := 				\
	arm/Dockerfile			\
	x86/Dockerfile			\

build: build-arm build-x86
push: push-arm push-x86

build-%: %/Dockerfile $(wildcard */*)
	chmod 0644 patches/*
	chmod 0755 *-scripts/*
	docker build -f $< -t $(TAG):$(VERSION)-$* .

push-%: build-%
	docker push $(TAG):$(VERSION)-$*

%/Dockerfile: Dockerfile.in %/.gitignore
	sed -e 's/@GHC_TARGET_ARCH@/$*/' $< > $@
