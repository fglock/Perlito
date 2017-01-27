use v5;
use strict;
use feature 'say';

say '1..21';
my %a;
say 'ok 1 - create hash';
$a{abc} = 3;
say 'ok 2 - set element';
if ($a{abc} != 3) {
    print 'not '
}
say 'ok 3 - fetch element # ', $a{abc};

$a{123} = 456;
say '# values: ', values %a;
say '# keys:   ', keys %a;

my %a1 = (a => 2); 
if ($a1{a} ne 2) {
    print 'not '
}
say "ok 4 - assign list to hash # {%a1}";

my %b1 = %a1; 
if ($b1{a} ne 2) {
    print 'not '
}
say "ok 5 - assign hash to hash # {%b1}";
$b1{a} = 5;
print 'not '
    unless $a1{a} == 2;
say "ok 6 - hash copy";
print 'not '
    unless $b1{a} == 5;
say "ok 7 - hash copy";
$b1{a} = 2;


my $c1 = { %b1, b => 3 };
if ($c1->{a} ne 2 || $c1->{b} ne 3) {
    print 'not '
}
say "ok 8 - interpolate hash in hash composer "; # {$c1};

print 'not ' if defined $c1->{c};
say "ok 9 - undefined item";

print 'not ' if !defined $c1->{b};
say "ok 10 - defined item";

$c1->{c} = 4;
print 'not ' if !defined $c1->{c};
say "ok 11 - defined item";

delete $c1->{c};
print 'not ' if exists $c1->{c};
say "ok 12 - delete item";

# autoquote

my %v;
sub x1 () { 1230 } $v{x1()} = 120;        # '1230'     => 120
sub x2 () { 1231 } $v{x2} = 121;          # 'x2'       => 121
sub x3 () { 1232 } $v{main::x3} = 122;    # '1232'     => 122

{
    no strict 'subs';
    $v{main::x4} = 123;                       # 'main::x4' => 123
}

print 'not ' if $v{'1230'}     != 120; say "ok 13 - no autoquote for function call with parenthesis";
print 'not ' if $v{'x2'}       != 121; say "ok 14 - autoquote for bareword without colons";
print 'not ' if $v{'1232'}     != 122; say "ok 15 - no autoquote for function call without parenthesis";
print 'not ' if $v{'main::x4'} != 123; say "ok 16 - autoquote for bareword with colons";

my %delete_from;
my $non_exist = delete($delete_from{'foo'});
print 'not ' if defined($non_exist); say "ok 17 - delete non-existent elem should return undef";

my %delete_with_val = ('d' => '24', 'e' => '100');
my $key = delete($delete_with_val{'d'});
print 'not ' if ($key ne '24'); say "ok 18 - delete returns the key";
my $key2 = $delete_with_val{'d'};
print 'not ' if defined($key2); say "ok 19 - delete deletes the key.";

{
    $a = undef;
    # $ perl -e ' use Data::Dumper;  $$a[0]; print Dumper $a; '
    $$a{aa};
    if (ref($a) ne 'HASH') {
        print 'not '
    }
    say 'ok 20 - deref to hash autovivifies';

    $a = undef;
    # $ perl -e ' use Data::Dumper;  $a->{aa}; print Dumper $a; '
    $a->{aa};
    if (ref($a) ne 'HASH') {
        print 'not '
    }
    say 'ok 21 - deref to hash autovivifies';
}

