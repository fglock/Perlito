use v6;

class Main {

    use MiniPerl6::Lisp::Emitter;

    say '1..4';
    my $m := MiniPerl6::Grammar.var_ident( '$abc', 0);
    say '# Ast is:        ', $m.perl;
    say '# code is:  ', ($$m).perl;
    say '# emit_lisp is:  ', ($$m).emit_lisp;
    if (($$m).emit_lisp) eq 'sv-abc' {
        say 'ok 1';
    }
    else {
        say 'not ok 1';
    }

    $m := MiniPerl6::Grammar.val_buf( '"abc"', 0);
    say '# Ast is:        ', $m.perl;
    say '# code is:  ', ($$m).perl;
    if (($$m).emit_lisp) eq '"abc"' {
        say 'ok 2';
    }
    else {
        say 'not ok 2';
    }

    $m := MiniPerl6::Grammar.exp_stmts( '123; 456 }', 0);
    say '# Ast is:        ', $m.perl;
    say '# code is:  ', ($$m).perl;
    if $$m {
        say 'ok 3';
    }
    else {
        say 'not ok 3';
    }

    $m := MiniPerl6::Grammar.comp_unit( 'class Main { 123 }', 0);
    say '# Ast is:        ', $m.perl;
    # say '# code is:  ', ($$m).emit_lisp;
    if ($$m).emit_lisp {
        say 'ok 4';
    }
    else {
        say 'not ok 4';
    }

}

