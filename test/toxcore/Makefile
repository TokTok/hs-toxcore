CFLAGS	:=					\
	-std=gnu99				\
	-Wall					\
	-DCONFIGURED=1				\
	-Imsgpack-c/include			\
	-Itoxcore/toxcore			\

CFLAGS	+= $(foreach d,$(shell find libsodium/src/libsodium -type d),-I$d)

LDFLAGS	:= -pthread
ifeq ($(shell echo 'int main(){}' | $(CC) -lrt -xc -o /dev/null - && echo true),true)
LDFLAGS	+= -lrt
endif

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

# All the possible test flavours.
PROGRAMS :=			\
	test_main-cov		\
	test_main-cov-asan	\
	test_main-vanilla	\
	test_main-vanilla-asan

ifneq ($(shell which afl-clang),)
PROGRAMS +=			\
	fuzz_main-afl		\
	fuzz_main-afl-asan
endif

all: $(PROGRAMS)

master: fuzz_main-afl test-findings
	afl-fuzz -i$(TEST_INPUTS) -o test-findings -M master ./$<

slave%: fuzz_main-afl test-findings
	afl-fuzz -i test-inputs -o test-findings -S slave$* ./$<

%-afl: %.c $(SOURCES) $(HEADERS)
	afl-clang $(CFLAGS) -O3 $(filter %.c,$^) $(LDFLAGS) -o $@

%-afl-asan: %.c $(SOURCES) $(HEADERS)
	afl-clang $(CFLAGS) -O3 -fsanitize=address $(filter %.c,$^) $(LDFLAGS) -o $@

%-cov: %.c $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) -fprofile-arcs -ftest-coverage $(filter %.c,$^) $(LDFLAGS) -o $@

%-cov-asan: %.c $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) -fprofile-arcs -ftest-coverage -fsanitize=address $(filter %.c,$^) $(LDFLAGS) -o $@

%-vanilla: %.c $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) $(filter %.c,$^) $(LDFLAGS) -o $@

%-vanilla-asan: %.c $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) -fsanitize=address $(filter %.c,$^) $(LDFLAGS) -o $@

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
	rm -rf $(wildcard *_main-* *.dSYM *.gcno *.gcda $(VERSION_H) test-findings/*)
