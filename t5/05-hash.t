use v5;

say '1..9';
my %a;
say 'ok 1 - create hash';
$a{'abc'} = 3;
say 'ok 2 - set element';
if $a{'abc'} != 3 {
    print 'not '
}
say 'ok 3 - fetch element # ', $a{'abc'};

$a{123} = 456;
say '# values: ', %a.values;
say '# keys:   ', %a.keys;

my %a1 = (a => 2); 
if $a1{'a'} ne 2 {
    print 'not '
}
say "ok 4 - assign list to hash # {%a1.perl}";

my %b1 = %a1; 
if $b1{'a'} ne 2 {
    print 'not '
}
say "ok 5 - assign hash to hash # {%b1.perl}";

my $c1 = { %b1, b => 3 };
if $c1->{'a'} ne 2 || $c1->{'b'} ne 3 {
    print 'not '
}
say "ok 6 - interpolate hash in hash composer # {$c1.perl}";

print 'not ' if defined $c1->{'c'};
say "ok 7 - undefined item";

print 'not ' if !defined $c1->{'b'};
say "ok 8 - defined item";

$c1->{'c'} = 4;
print 'not ' if !defined $c1->{'c'};
say "ok 9 - defined item";

# TODO
# my $d1 = [ $c1.kv, 3 ];
# if $d1[0] ne 'a' || $d1[1] != 2 || $d1[2] ne 'b' {
#     print 'not '
# }
# say "ok 7 - interpolate hash in array composer # {$d1.perl}";

