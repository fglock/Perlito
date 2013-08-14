
all: rebuild test

test:
	perl ../perlito5.pl -I../src5/lib -Cast-perl5 -e 'package Some::Thing; sub foo { my $$something = "Hello World"; return $$something }'
	test -d Some-Thing || h2xs --skip-ppport -An Some::Thing
	perl ../perlito5.pl -I../src5/lib -Cxs -e 'package Some::Thing; sub foo { my $$something = "Hello World"; return $$something }' > Some-Thing/Thing.xs
	cd Some-Thing && perl Makefile.PL && make test


rebuild:
	cd ../ && make build-5to5

