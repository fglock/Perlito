use v6;

class Main {
    use MiniPerl6::Eval;

    say '1..1';

    my $m := ::Val::Num( num => 123 );
    if ($m.eval) eq 123 {
        say 'ok 1';
    }
    else {
        say 'not ok 1';
    }

    $m := ::Apply(
                code      => 'say',
                namespace => '',
                arguments => [ ::Val::Buf( buf => '# ok print()' ) ],
            );
    $m.eval;

}
