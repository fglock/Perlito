use v6;

class Main {

    use MiniPerl6::Lisp::Emitter;

    say '1..4';
    my $m := MiniPerl6::Grammar.exp_stmts( 'abc.meth()', 0);
    say '# Ast is:        ', $m.perl;
    say '# code is:  ', ($$m).perl;
    say '# emit is:  ', (($$m)[0]).emit_lisp;
    if ((($$m)[0]).emit_lisp) eq '(sv-meth (proto-mp-abc) )' {
        say 'ok 1';
    }
    else {
        say 'not ok 1';
    }

    $m := MiniPerl6::Grammar.exp_stmts( 'abc.meth', 0);
    say '# Ast is:        ', $m.perl;
    say '# code is:  ', ($$m).perl;
    say '# emit is:  ', (($$m)[0]).emit_lisp;
    if ((($$m)[0]).emit_lisp) eq '(sv-meth (proto-mp-abc) )' {
        say 'ok 2';
    }
    else {
        say 'not ok 2';
    }

    $m := MiniPerl6::Grammar.exp_stmts( 'abc.meth 123', 0);
    say '# Ast is:        ', $m.perl;
    say '# code is:  ', ($$m).perl;
    if $$m {
        say 'ok 3';
    }
    else {
        say 'not ok 3';
    }

    $m := MiniPerl6::Grammar.exp_stmts( 'abc.meth(123)', 0);
    say '# Ast is:        ', $m.perl;
    say '# code is:  ', (($$m)[0]).emit_lisp;
    if (($$m)[0]).emit_lisp {
        say 'ok 4';
    }
    else {
        say 'not ok 4';
    }

}

