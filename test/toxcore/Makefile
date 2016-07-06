CFLAGS	:= -Imsgpack-c/include -Itoxcore/toxcore -lsodium -O3
SOURCES	:= $(wildcard *.c msgpack-c/src/*.c toxcore/toxcore/*.c)

ifeq ($(wildcard test-findings/master/fuzzer_stats),)
TEST_INPUTS := test-inputs
else
TEST_INPUTS := -
endif

check: check-program

check-%: test-% test-findings
	afl-fuzz -i$(TEST_INPUTS) -o test-findings -M master ./$<

slave%: test-program test-findings
	afl-fuzz -i test-inputs -o test-findings -S slave$* ./$<

test-program: $(SOURCES)
	afl-clang $(CFLAGS) $^ -o $@

test-program-asan: driver.c methods.c $(SOURCES)
	afl-clang $(CFLAGS) -fsanitize=address $^ -o $@

test-findings: /Volumes/RAM\ Disk
	mkdir -p "$</$@"
	ln -sf "$</$@" $@

/Volumes/RAM\ Disk:
	diskutil erasevolume HFS+ 'RAM Disk' `hdiutil attach -nomount ram://204800`

clean:
	rm -rf test-program test-program-asan test-program.dSYM test-findings/*
