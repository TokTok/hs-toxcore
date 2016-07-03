CFLAGS := -Imsgpack-c/include -Itoxcore/toxcore -lsodium
SOURCES := $(wildcard *.c msgpack-c/src/*.c toxcore/toxcore/*.c)

ifeq ($(wildcard test-findings/master/fuzzer_stats),)
TEST_INPUTS := test-inputs
else
TEST_INPUTS := -
endif

check: test-program test-findings
	afl-fuzz -i$(TEST_INPUTS) -o test-findings -M master ./$<

slave%: test-program test-findings
	afl-fuzz -i test-inputs -o test-findings -S slave$* ./$<

check1: test-program-asan test-inputs/initial
	./$< < test-inputs/initial

test-program: $(SOURCES)
	afl-clang $(CFLAGS) -O3 $^ -o $@

test-program-asan: driver.c methods.c $(SOURCES)
	clang $(CFLAGS) -fsanitize=address $^ -o $@

test-findings: /Volumes/RAM\ Disk
	mkdir "$</$@"
	ln -sf "$</$@" $@

/Volumes/RAM\ Disk:
	diskutil erasevolume HFS+ 'RAM Disk' `hdiutil attach -nomount ram://204800`

clean:
	rm -rf test-program test-program-asan test-program.dSYM
