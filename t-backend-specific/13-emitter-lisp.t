use v6;

class Main {
    use Perlito::Lisp::Emitter;

    say '1..7';
    my $m := ::Val::Num( num => 123 );
    # say '# Ast is:        ', $m.perl;
    say '# code is:  ', $m.emit_lisp;
    if ($m.emit_lisp) eq '123' {
        say 'ok 1';
    }
    else {
        say 'not ok 1';
    }

    $m := ::Val::Buf( buf => 'abc' );
    say '# value is ', $m.emit_lisp;
    if ($m.emit_lisp) eq '"abc"' {
        say 'ok 2';
    }
    else {
        say 'not ok 2';
    }

    $m := Perlito::Grammar.word( 'abcdef', 2 );
    # say 'match scalar: ', $$m;
    if ($$m) eq 'c' {
        say 'ok 3';
    }
    else {
        say 'not ok 3';
    }

    $m := ::Lit::Array( array1 => [ ::Val::Int( int => 1 ), ::Val::Buf( buf => "2" ) ] );
    say '# array:  ', $m.emit_lisp;

# token

    token num1 { 5 }
    $m := Main.num1( '5', 0 );
    say '# match scalar: ', $$m;
    if ($$m) eq '5' {
        say 'ok 4';
    }
    else {
        say 'not ok 4';
    }

# make

    token num2 { 5 { make 123 } }
    $m := Main.num2( '5', 0 );
    say '# match scalar: ', $$m;
    # say '# match capture: ', $m.capture;
    if ($$m) == 123 {
        say 'ok 5';
    }
    else {
        say 'not ok 5';
    }

# named subcapture

    token num3 { 5 <Perlito::Grammar.word> 2 }
    $m := Main.num3( '5x2', 0 );
    say '# match scalar: ', $$m;
    # say '# match capture: ', $m.capture;
    my $cap := scalar( ($m.hash){'Perlito::Grammar.word'} );
    say '# match named capture: ', $cap;
    say '# bool value (true): ', ?$m;
    # say '# match object:      ', $m.perl;
    if ($cap eq 'x') {
        say 'ok 6';
    }
    else {
        say 'not ok 6';
    }
    $m := Main.num3( '5?2', 0 );
    say '# bool value (false): ', ?$m;
    if ($m) {
        say 'not ok 7';
    }
    else {
        say 'ok 7';
    }

}

