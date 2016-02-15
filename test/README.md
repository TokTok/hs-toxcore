# Systems Under Test

This is the test program directory. All executable regular files in this
directory will be assumed to implement the test protocol via their stdin/stdout
and will be tested by the test runner.

## How to write a SUT

A System Under Test is a program that reads test commands from `stdin` and
writes test results to `stdout`. The format is as follows (lengths are in
bytes):

Length    | Type   | Content
--------- | ------ | -------
`8`       | Int    | Length of test name in bytes: `$length`
`$length` | String | UTF-8 encoded (in fact ASCII only) test name
`[0,]`    | Data   | Arbitrary test data

The test data depends on which test is ran. Some tests have sub-tests separated
by a single space. The up-to-date tests can be found in the
[Test](../src/Network/Tox/ExternalTest/Test.hs) module. The `Test` data type
lists the test names with their inputs and expected outputs.

A very simple [reference implementation](../src/TestClient.hs) can be used to
compare your implementation against.

## Caveats

The Test Runner will execute the test program about 100 times per test, so the
program should be fast to start up. If you have slow startup time, or you're
using an execution environment like the JVM that requires JIT warmup time,
you're probably better off writing a small external tool that communicates with
a longer-running instance of your SUT.
