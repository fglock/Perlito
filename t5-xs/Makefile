
CODE =	package Some::Thing;
#CODE +=	sub foo { my $$something = 21; return $$something * 2; }
CODE +=	sub foo { warn( "MOO" ) }
#CODE +=	sub foo { my $$something = [qw[ Hello World ]]; return $$something }
#CODE +=	sub bar { my @something = qw( Hello World ); return \@something }

all: rebuild test

test:
	perl ../perlito5.pl -I../src5/lib -Cast-perl5 -e '${CODE}'
	test -d Some-Thing || h2xs --skip-ppport -An Some::Thing
	perl ../perlito5.pl -I../src5/lib -Cxs -e '${CODE}' > Some-Thing/Thing.xs
	cd Some-Thing && perl Makefile.PL && make test TEST_VERBOSE=1


rebuild:
	cd ../ && make build-5to5

