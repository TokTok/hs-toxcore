CFLAGS	:=					\
	-DCONFIGURED=1				\
	-Imsgpack-c/include			\
	-Itoxcore/toxcore			\

CFLAGS	+= $(foreach d,$(shell find libsodium/src/libsodium -type d),-I$d)

LDFLAGS	:= -pthread

SOURCES	:= $(filter-out %_main.c,$(wildcard *.c msgpack-c/src/*.c toxcore/toxcore/*.c))
SOURCES	+= $(shell find libsodium/src -name "*.c")

VERSION_H := libsodium/src/libsodium/include/sodium/version.h
HEADERS	:= $(VERSION_H)
HEADERS += $(shell find . -name "*.h")

ifeq ($(wildcard test-findings/master/fuzzer_stats),)
TEST_INPUTS := test-inputs
else
TEST_INPUTS := -
endif

PROGRAMS :=		\
	test_main-cov	\
	test_main-asan	\

ifneq ($(shell which afl-clang),)
PROGRAMS +=		\
	fuzz_main-afl	\
	fuzz_main-asan
endif

all: $(PROGRAMS)

master: fuzz_main-afl test-findings
	afl-fuzz -i$(TEST_INPUTS) -o test-findings -M master ./$<

slave%: fuzz_main-afl test-findings
	afl-fuzz -i test-inputs -o test-findings -S slave$* ./$<

%-afl: %.c $(SOURCES) $(HEADERS)
	afl-clang $(CFLAGS) $(LDFLAGS) -O3 $(filter %.c,$^) -o $@

%-afl-asan: %.c $(SOURCES) $(HEADERS)
	afl-clang $(CFLAGS) $(LDFLAGS) -O3 -fsanitize=address $(filter %.c,$^) -o $@

%: %.c $(SOURCES) $(HEADERS)
	$(LINK.c) $(filter %.c,$^) -o $@

%-asan: %.c $(SOURCES) $(HEADERS)
	$(LINK.c) -fsanitize=address $(filter %.c,$^) -o $@

%-cov: %.c $(SOURCES) $(HEADERS)
	$(LINK.c) -fprofile-arcs -ftest-coverage $(filter %.c,$^) -o $@

$(VERSION_H): Makefile
	echo '#define SODIUM_VERSION_STRING "<version>"' > $@
	echo '#define SODIUM_LIBRARY_VERSION_MAJOR 0' >> $@
	echo '#define SODIUM_LIBRARY_VERSION_MINOR 0' >> $@

test-findings: /Volumes/RAM\ Disk
	mkdir -p "$</$@"
	ln -sf "$</$@" $@

/Volumes/RAM\ Disk:
	diskutil erasevolume HFS+ 'RAM Disk' `hdiutil attach -nomount ram://204800`

clean:
	rm -rf $(wildcard *_main *_main-* *.dSYM *.gcno *.gcda $(VERSION_H) test-findings/*)
