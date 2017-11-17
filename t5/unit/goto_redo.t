use feature 'say';

say '1..1';

# goto working as "redo"

my $do = 0;
my $pass = 0;

  {
HERE:
    print "# start block\n";
    $pass++;
    print "ok 1\n" if $do;
    last if $do;
    $do = 1;
    goto HERE;
  }

print "not " if $pass != 2;
print "ok 2 - loop works\n";

