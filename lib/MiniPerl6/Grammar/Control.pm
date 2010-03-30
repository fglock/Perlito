
use v6;

grammar MiniPerl6::Grammar {


token control {
    | <ctrl_return> { make $$<ctrl_return> }   # return 123;
    | <ctrl_leave>  { make $$<ctrl_leave>  }   # last; break;
    | <if>     { make $$<if>     }   # 1 ?? 2 !! 3
    | <when>   { make $$<when>   }   # when 3 { ... }
    | <for>    { make $$<for>    }   # $x.map(-> $i {...})
    | <while>  { make $$<while>  }   # while ... { ... }
    | <loop>   { make $$<loop>   }   # loop { ... }
    | <apply>  { make $$<apply>  }   # $obj($arg1, $arg2)
 #  | <call>   { make $$<call>   }   # $obj.method($arg1, $arg2)
};

token if {
    if <.ws>  <exp>  <.opt_ws>
    \{ <.opt_ws> <exp_stmts> <.opt_ws> \} 
    [
        <.opt_ws>
        else <.opt_ws> 
        \{ <.opt_ws> <exp_stmts2> <.opt_ws> \}
        { 
            make If.new( 
                cond => $$<exp>, 
                body => $$<exp_stmts>, 
                otherwise => $$<exp_stmts2>,
            );
        }
    |
        <.opt_ws>
        els <if> 
        { 
            make If.new( 
                cond => $$<exp>, 
                body => $$<exp_stmts>, 
                otherwise => [ $$<if> ],
            );
        }
    |
        { 
            make If.new( 
                cond => $$<exp>, 
                body => $$<exp_stmts>, 
                otherwise => [ ],
             ) 
        }
    ]
};

token when {
    when <.ws> <exp_seq> <.opt_ws> \{ <.opt_ws> <exp_stmts> <.opt_ws> \}
    { make When.new( parameters => $$<exp_seq>, body => $$<exp_stmts> ) }
};

token for {
    for <.ws> <exp> <.opt_ws> '->' <.opt_ws> <var_ident> <.ws> \{ <.opt_ws> <exp_stmts> <.opt_ws> \}
    { make For.new( cond => $$<exp>, topic => $$<var_ident>, body => $$<exp_stmts> ) }
};

token while {
    while <.ws> <exp> <.ws> \{ <.opt_ws> <exp_stmts> <.opt_ws> \}
    { make While.new( cond => $$<exp>, body => $$<exp_stmts> ) }
};

token loop {
    loop <.ws> 
        [ 
            \{ <.opt_ws> <exp_stmts> <.opt_ws> \}
            { make While.new( cond => Val::Bit.new( bit => 1 ), body => $$<exp_stmts> ) }
        |
            { die 'loop(;;){} not implemented' } 
            # \( <exp> <.opt_ws> ';' <.opt_ws> <exp> <.opt_ws> ';' <.opt_ws> <exp> <.opt_ws> \) 
            # \{ <.opt_ws> <exp_stmts> <.opt_ws> \}
        ]
}

token ctrl_leave {
    leave
    { make Leave.new() }
};

token ctrl_return {
    return [ <before '(' > | <.ws> ] <exp>
    { make Return.new( result => $$<exp> ) }
    |
    return 
    { make Return.new( result => Val::Undef.new() ) }
};

}


=begin

=head1 NAME 

MiniPerl6::Grammar - Grammar for MiniPerl6

=head1 SYNOPSIS

    my $match = $source.parse;
    ($$match).perl;    # generated MiniPerl6 AST

=head1 DESCRIPTION

This module generates a syntax tree for the MiniPerl6 compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006, 2009 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
