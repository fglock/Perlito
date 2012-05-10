use v5;

package Perlito5::Grammar::Sigil;

use Perlito5::Precedence;

Perlito5::Precedence::add_term( '$'  => sub { Perlito5::Grammar::Sigil->term_sigil($_[0], $_[1]) } );
Perlito5::Precedence::add_term( '@'  => sub { Perlito5::Grammar::Sigil->term_sigil($_[0], $_[1]) } );
Perlito5::Precedence::add_term( '%'  => sub { Perlito5::Grammar::Sigil->term_sigil($_[0], $_[1]) } );
Perlito5::Precedence::add_term( '&'  => sub { Perlito5::Grammar::Sigil->term_sigil($_[0], $_[1]) } );
Perlito5::Precedence::add_term( '*'  => sub { Perlito5::Grammar::Sigil->term_sigil($_[0], $_[1]) } );


# the special variables list
# obtained with:
# $ perldoc -u perlvar | perl -ne ' /^\s*$/ && next; if (/^=item\s+([^\n]+)/) { push @item, $1; print "@item - $_" } else { if (@item) { push @xx, [@item]; print "push\n"; @item = () } }; END {use Data::Dumper; print Dumper \@xx} '

# $ perldoc -u perlvar | perl -ne ' /^\s*$/ && next; if (/^=item\s+([^\n]+)/) { push @item, $1; print "@item - $_" } else { if (@item) { push @xx, grep { /^[\@\$\%][^a-zA-Z0-9]$/ } @item; print "push\n"; @item = () } }; END {use Data::Dumper; print "$_  => 1,\n" for @xx} '

my %special_var = (
    '$_'  => 1,
    '$&'  => 1,
    '$`'  => 1,
    '$\''  => 1,
    '$+'  => 1,
    '@+'  => 1,
    '%+'  => 1,
    '$.'  => 1,
    '$/'  => 1,
    '$|'  => 1,
    '$,'  => 1,
    '$\\'  => 1,
    '$"'  => 1,
    '$;'  => 1,
    '$%'  => 1,
    '$='  => 1,
    '$-'  => 1,
    '@-'  => 1,
    '%-'  => 1,
    '$~'  => 1,
    '$^'  => 1,
    '$:'  => 1,
    '$?'  => 1,
    '$!'  => 1,
    '%!'  => 1,
    '$@'  => 1,
    '$$'  => 1,
    '$<'  => 1,
    '$>'  => 1,
    '$('  => 1,
    '$)'  => 1,
    '$['  => 1,
    '$]'  => 1,
    '@_'  => 1,
    # '$#'  => 1,   #  "$# is no longer supported"
    '$*'  => 1,

    '$#+'  => 1,    # $# + @+
    '$#-'  => 1,    # $# + @-
    '$#_'  => 1,    # $# + @_
);

sub term_special_var {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
    my $len = 0;

    # TODO:
    #
    # this is ok:
    #  ' $ {!} ', ' @ {+} ', ' $#{+} '
    #  ' @{ x ->[10] } '
    #  ' ${v {int} -> {t}} '
    #
    # syntax errors:
    #  ' $# {+} ', ' $ #{+} ', ' @ { + } '
    #  ' @x->[10] '
    #
    # this is never a function call:
    #  ' ${main::x} '
    #

    my $s = substr( $str, $pos, 3 );
    if ( $s eq '$#[' ) {
        # special case: $# is not valid, but @# is ok
        $len = 2;
    }
    elsif ( exists $special_var{$s} ) {
        $len = 3;
    }
    else {
        $s = substr( $str, $pos, 2 );
        if ( exists $special_var{$s} ) {
            $len = 2;
        }
    }
    if ( $len ) {
        my $c0 = substr( $str, $pos + $len - 1, 1 );
        my $c1 = substr( $str, $pos + $len, 1 );
        if  ( 
                ( $c0 eq '$' || $c0 eq '@' || $c0 eq '%' || $c0 eq '*' || $c0 eq '&' )
            &&  
                ( $c1 eq '$' || $c1 eq '@' || $c1 eq '%' || $c1 eq '*' || $c1 eq '&' 
                || ( $c1 ge 'a' && $c1 le 'z' )
                || ( $c1 ge 'A' && $c1 le 'Z' )
                || ( $c1 ge '0' && $c1 le '9' )
                )
            ) 
        {
            # TODO - this needs more testing
            # looks like a prefix operator, not a special var
        }
        else {
            return {
               str     => $str,
               from    => $pos,
               to      => $pos + $len,
               capture => [ 'term', 
                                Perlito5::AST::Var->new(
                                        sigil       => substr($s, 0, $len - 1),
                                        namespace   => '',
                                        name        => substr($s, $len - 1, 1)
                                    )
                          ]
            };
        }
    }
    return 0;
}

token var_sigil_or_pseudo     { '$#' | \$ |\% |\@ |\& | \* };

token term_sigil {
    <var_sigil_or_pseudo> <.Perlito5::Grammar::Space.opt_ws>
        [ '{' <.Perlito5::Grammar::Space.opt_ws> 
            [
            | <Perlito5::Grammar.optional_namespace_before_ident> <Perlito5::Grammar.var_name> 
                <.Perlito5::Grammar::Space.opt_ws>

                    {
                        # we are parsing:  ${var}  ${var{index}}
                        # create the 'Var' object
                        $MATCH->{capture} = Perlito5::AST::Var->new(
                            sigil       => Perlito5::Match::flat($MATCH->{var_sigil_or_pseudo}),
                            namespace   => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.optional_namespace_before_ident"}),
                            name        => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.var_name"}),
                        );
                        # hijack some string interpolation code to parse the subscript
                        $MATCH = Perlito5::Grammar::String->double_quoted_var_with_subscript($MATCH);
                        $MATCH->{capture} = [ 'term', $MATCH->{capture} ];
                    }
                    <.Perlito5::Grammar::Space.opt_ws>
                    '}'


            | '^' <Perlito5::Grammar.var_name> '}'
                    { $MATCH->{capture} = [ 'term', 
                            Perlito5::AST::Var->new(
                                    sigil       => Perlito5::Match::flat($MATCH->{var_sigil_or_pseudo}),
                                    namespace   => 'main',
                                    name        => '^' . Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.var_name"}),
                                )
                        ]
                    }
            | <Perlito5::Expression.curly_parse>   '}'
                { $MATCH->{capture} = [ 'term',  
                        Perlito5::AST::Apply->new( 
                                'arguments' => [ Perlito5::Match::flat($MATCH->{"Perlito5::Expression.curly_parse"}) ],
                                'code'      => 'prefix:<' . Perlito5::Match::flat($MATCH->{var_sigil_or_pseudo}) . '>', 
                                'namespace' => ''
                            )
                    ] 
                }
            ]
        | '^' <Perlito5::Grammar.word>
                { $MATCH->{capture} = [ 'term', 
                        Perlito5::AST::Var->new(
                                sigil       => Perlito5::Match::flat($MATCH->{var_sigil_or_pseudo}),
                                namespace   => 'main',
                                name        => '^' . Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.word"}),
                            )
                    ]
                }
        | <Perlito5::Grammar.optional_namespace_before_ident> <Perlito5::Grammar.var_name>
                { $MATCH->{capture} = [ 'term', 
                        Perlito5::AST::Var->new(
                                sigil       => Perlito5::Match::flat($MATCH->{var_sigil_or_pseudo}),
                                namespace   => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.optional_namespace_before_ident"}),
                                name        => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.var_name"}),
                            )
                    ]
                }
        | <before '$'> <term_sigil>
                { $MATCH->{capture} = [ 'term',  
                        Perlito5::AST::Apply->new( 
                                'arguments' => [ $MATCH->{term_sigil}{capture}[1] ],
                                'code'      => 'prefix:<' . Perlito5::Match::flat($MATCH->{var_sigil_or_pseudo}) . '>', 
                                'namespace' => ''
                            )
                    ] 
                }
        ]
    | <term_special_var>
            { $MATCH->{capture} = $MATCH->{term_special_var}->{capture} }
};


1;

=begin

=head1 NAME

Perlito5::Grammar::Sigil - Parser module for Perlito

=head1 SYNOPSIS

=head1 DESCRIPTION

This module parses source code for Perl 5 statements and generates Perlito5 AST.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2010, 2011, 2012 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

