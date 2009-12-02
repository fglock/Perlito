use v6-alpha;

class Main {
    say '1..2';
    my $a := do { 3 };
    say 'ok 1 - do block';
    if $a != 3 {
        print 'not '
    }
    say 'ok 2 - do value';
}
