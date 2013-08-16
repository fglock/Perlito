
# keep names uppercase and a single word to keep the makefile simple
TESTS +=	ReturnHelloWorld
TESTS +=	ReturnArray
TESTS +=	ReturnArrayRef
TESTS +=	Warn

all: rebuild test

test: ${TESTS:%=test-%}

# to strip leading "test-" in target rule from target name
TEST = ${@:test-%=%}

# prepend "test-" to all test targets
${TESTS:%=test-%}:
	perl ../perlito5.pl -I../src5/lib -Cast-perl5 ${TEST}.pl
	test -d ${TEST} || ( \
		h2xs --skip-ppport -An ${TEST} && \
		cd ${TEST} && echo -e "use Test::More tests => 2;\nBEGIN { use_ok('${TEST}') }\nok( ${TEST}::test() );" > \
		    t/${TEST}.t \
	)
	perl ../perlito5.pl -I../src5/lib -Cxs ${TEST}.pl > ${TEST}/${TEST}.xs
	cd ${TEST} && perl Makefile.PL && make test TEST_VERBOSE=1

rebuild:
	cd ../ && make build-5to5

clean:
	-for D in ${TESTS}; do \
	    ( cd $${D} && make clean; rm -f Makefile.old ); \
	done
