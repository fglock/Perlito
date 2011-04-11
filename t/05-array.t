use v6;

class Main {
    say '1..7';
    my @a;
    say 'ok 1 - create array';
    @a[1] = 3;
    say 'ok 2 - set element';
    if @a[1] != 3 {
        print 'not '
    }
    say 'ok 3 - fetch element # ', @a[1];

    my @x = ( 3, 4 ); 
    @a = ( 1, @x, 2 ); 
    if @a[1] != 3 {
        print 'not '
    }
    say 'ok 4 - interpolate array # ', @a;

    my $x = [ 5, 6 ];
    my $v = $x[1];
    if $v != 6 {
        print 'not '
    }
    say 'ok 5 - array in a scalar var # ', $v;
    $x[1] = 7;
    if $x[1] != 7 {
        print 'not '
    }
    say 'ok 6 - array in a scalar var # ', $x[1];

    {
        my $v;
        $v[2] = 8;
        if $v[2] != 8 {
            print 'not '
        }
        say 'ok 7 - array in a scalar var # ', $v[2];
    }
}
