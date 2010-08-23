
use v6;

grammar MiniPerl6::Grammar {

token unless {
    unless <.ws> <exp>  
        { 
            my $body = ($$<exp>){'end_block'};
            if !(defined($body)) {
                die "Missing code block in 'unless'";
            }
            make If.new( 
                cond => ($$<exp>){'exp'}, 
                body => [ ], 
                otherwise => $body,
             ) 
        }
}

token if {
    if <.ws> <exp>  
    [
        <.opt_ws>
        else <exp2> 
        { 
            my $body = ($$<exp>){'end_block'};
            my $otherwise = ($$<exp2>){'exp'};
            if !(defined($body)) {
                die "Missing code block in 'if'";
            }
            if !(defined($otherwise)) {
                die "Missing code block in 'else'";
            }
            if $otherwise.isa('Lit::Hash') {
                $otherwise = Lit::Block.new( stmts => $otherwise.hash1 );
            }
            make If.new( 
                cond      => ($$<exp>){'exp'}, 
                body      => $body, 
                otherwise => $otherwise,
            )
        }
    |
        <.opt_ws>
        els <if> 
        { 
            my $body = ($$<exp>){'end_block'};
            if !(defined($body)) {
                die "Missing code block in 'if'";
            }
            make If.new( 
                cond => ($$<exp>){'exp'}, 
                body => $body, 
                otherwise => Lit::Block.new( stmts => [ $$<if> ] ),
            )
        }
    |
        { 
            my $body = ($$<exp>){'end_block'};
            if !(defined($body)) {
                die "Missing code block in 'if'";
            }
            make If.new( 
                cond => ($$<exp>){'exp'}, 
                body => $body, 
                otherwise => [ ],
             ) 
        }
    ]
}

token when {
    when <.ws> <exp> 
    { 
        my $body = ($$<exp>){'end_block'};
        if !(defined($body)) {
            die "Missing code block in 'when'";
        }
        make When.new( 
                parameters => ($$<exp>){'exp'}, 
                body       => $body ) 
    }
}

token for {
    for <.ws> <exp> 
    { 
        my $body = ($$<exp>){'end_block'};
        if !(defined($body)) {
            die "Missing code block in 'when'";
        }
        make For.new( cond => ($$<exp>){'exp'}, topic => undef, body => $body ) 
    }
}

token while {
    while <.ws> <exp> 
    {
        my $body = ($$<exp>){'end_block'};
        if !(defined($body)) {
            die "Missing code block in 'while'";
        }
        make While.new( 
                cond => ($$<exp>){'exp'}, 
                body => $body ) 
    }
}

token loop {
    loop <.ws> <exp>
    {
        my $body = ($$<exp>){'end_block'};
        if !(defined($body)) {
            $body = ($$<exp>){'exp'};
            if $body.isa( 'Lit::Block' ) {
                make While.new( cond => Val::Bit.new( bit => 1 ), body => $body )
            }
            else {
                die "Missing code block in 'loop'";
            }
        }
        else {
            die "'loop' with parameters is not implemented";
        }
    }
}

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
