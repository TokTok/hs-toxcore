ifneq ($(wildcard ../spec/pandoc.mk),)
ifneq ($(shell which pandoc),)
DOCS	:= ../spec/spec.md
include ../spec/pandoc.mk
endif
endif

doc: $(DOCS)
../spec/spec.md: src/Network/Tox.lhs $(shell find src -name "*.lhs") ../spec/pandoc.mk ../spec/.md-style.rb Makefile
	echo '% The Tox Reference' > $@
	echo '' >> $@
	pandoc $< $(PANDOC_ARGS)							\
		-f latex+lhs								\
		-t $(FORMAT)								\
		| sed -e "s/’/'/g"	\
		| perl -pe 'BEGIN{undef $$/} s/\`\`\` sourceCode\n.*?\`\`\`\n\n//sg'	\
		>> $@
	if which mdl; then $(MAKE) -C $(@D) check; fi
	cd $(@D) && git diff --exit-code
