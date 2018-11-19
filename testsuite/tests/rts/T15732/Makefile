TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

MY_TEST_HC_OPTS=$(TEST_HC_OPTS) "-expand-response-files"

# We can set RTS options using response files. This test sets a GC flag.
T15732_01:
	'$(TEST_HC)' $(MY_TEST_HC_OPTS) -v0 --make -rtsopts T15732_01.hs
	./T15732_01 @T15732_01.response

# If the program was not compiled with -rtsopts and we pass RTS options
# at runtime, an error should be reported.
T15732_02:
	'$(TEST_HC)' $(filter-out -rtsopts, $(MY_TEST_HC_OPTS)) -v0 --make T15732_02.hs
	./T15732_02 @T15732_02.response &> /dev/null

# Recursive response files are OK. Also, @file args can appear in between
# +RTS...-RTS blocks.
## First, without -expand-response-files.
T15732_03a:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make T15732_03.hs -o T15732_03a
	./T15732_03a @T15732_03.response

## Then, with -expand-response-files.
T15732_03b:
	'$(TEST_HC)' $(MY_TEST_HC_OPTS) -v0 --make T15732_03.hs -o T15732_03b
	./T15732_03b @T15732_03.response

# RTS options occuring after --RTS are ignored, but response file arguments are
# still expanded.
## First, without -expand-response-files.
T15732_04a:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make T15732_04.hs -o T15732_04a
	./T15732_04a +RTS -F3 -RTS a b --RTS c +RTS -F5 -RTS @T15732_04.response

## Then, with -expand-response-files.
T15732_04b:
	'$(TEST_HC)' $(MY_TEST_HC_OPTS) -v0 --make T15732_04.hs -o T15732_04b
	./T15732_04b +RTS -F3 -RTS a b --RTS c +RTS -F5 -RTS @T15732_04.response

# Test if '--' and '--RTS' are handled correctly.
## First, without -expand-response-files.
T15732_05a:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make T15732_05.hs -o T15732_05a
	./T15732_05a --RTS +RTS -F3 -RTS a b c -- @T15732_05.response

## Then, with -expand-response-files.
T15732_05b:
	'$(TEST_HC)' $(MY_TEST_HC_OPTS) -v0 --make T15732_05.hs -o T15732_05b
	./T15732_05b --RTS +RTS -F3 -RTS a b c -- @T15732_05.response

# If -expand-response-files is not set, the RTS doesn't process the response files.
# getArgs and getArgsWithResponseFiles are distinct. Also, RTS flags in any
# response files don't have any effect, and are passed on to the program.
T15732_06:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make T15732_06.hs
	./T15732_06 a b c @T15732_06.response +RTS -F3 -RTS

# Test both parsers (the one in RTS, and GHC.ResponseFile) by using tests from
# the GCC source-code.
T15732_07:
	'$(TEST_HC)' $(MY_TEST_HC_OPTS) -v0 --make T15732_07.hs
	./T15732_07 @T15732_07.response

# Recursive response files don't cause an infinite loop.
## First, without -expand-response-files.
T15732_08a:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make T15732_08.hs -o T15732_08a
	! echo main | ./T15732_08a @T15732_08.response

## Then, with -expand-response-files.
T15732_08b:
	'$(TEST_HC)' $(MY_TEST_HC_OPTS) -v0 --make T15732_08.hs -o T15732_08b
	! echo main | ./T15732_08b @T15732_08.response

.PHONY: T15732_01 T15732_02 T15732_03 T15732_04a T15732_04b T15732_05a T15732_05b
	T15732_06 T15732_07 T15732_08a T15732_08b
