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
    '$_'  => '',
    '$&'  => '',
    '$`'  => '',
    '$\'' => '',
    '$+'  => '',
    '@+'  => '',
    '%+'  => '',
    '$.'  => '',
    '$/'  => '',
    '$|'  => '',
    '$,'  => '',
    '$\\' => '',
    '$"'  => '',
    '$;'  => '',
    '$%'  => '',
    '$='  => '',
    '$-'  => '',
    '@-'  => '',
    '%-'  => '',
    '$~'  => '',
    '$^'  => '',
    '$:'  => '',
    '$?'  => '',
    '$!'  => '',
    '%!'  => '',
    '$@'  => '',
    '$$'  => '',
    '$<'  => '',
    '$>'  => '',
    '$('  => '',
    '$)'  => '',
    '$['  => '',
    '$]'  => 'main',
    '@_'  => '',

    # '$*'  => '',  #  "$* is no longer supported"
    # '$#'  => 1,   #  "$# is no longer supported"

    '*_'  => '',
    '*&'  => '',
    '*`'  => '',
    '*\'' => '',
    '*+'  => '',
    '*.'  => '',
    '*/'  => '',
    '*|'  => '',
    '*,'  => '',
    '*\\' => '',
    '*"'  => '',
    '*;'  => '',
    '*%'  => '',
    '*='  => '',
    '*-'  => '',
    '*~'  => '',
    '*^'  => '',
    '*:'  => '',
    '*?'  => '',
    '*!'  => '',
    '*@'  => '',
    '*$'  => '',
    '*<'  => '',
    '*>'  => '',
    '*('  => '',
    '*)'  => '',
    '*['  => '',
    '*]'  => '',
    '*_'  => '',
    '**'  => '',
    '*#'  => '',

    '$#+' => '',    # $# + @+
    '$#-' => '',    # $# + @-
    '$#_' => '',    # $# + @_
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
                                        namespace   => $special_var{$s},
                                        name        => substr($s, $len - 1, 1)
                                    )
                          ]
            };
        }
    }
    return 0;
}

my %sigil = (
    '$'  => 1,
    '%'  => 1,
    '@'  => 1,
    '&'  => 1,
    '*'  => 1,
);

sub term_sigil {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];

    my $c1 = substr($str, $pos, 1);
    return unless exists $sigil{$c1};

    my $p = $pos + 1;
    my $sigil = $c1;
    if (substr($str, $pos, 2) eq '$#') {
        $sigil = '$#';
        $p++;
    }

    my $m = Perlito5::Grammar::Space->opt_ws($str, $p);
    $p = $m->{to};

    $c1 = substr($str, $p, 1);
    my $q = $p + 1;
    if ( $c1 eq '{' ) {
        #  ${ ...
        my $p = $q;
        $m = Perlito5::Grammar::Space->opt_ws($str, $p);
        $p = $m->{to};

        $m = Perlito5::Grammar->optional_namespace_before_ident( $str, $p );
        if ($m) {
            #  ${name  ...
            my $n = Perlito5::Grammar->var_name( $str, $m->{to} );
            if ($n) {
                my $spc = Perlito5::Grammar::Space->opt_ws($str, $n->{to});
                # we are parsing:  ${var}  ${var{index}}
                # create the 'Var' object
                $m->{capture} = Perlito5::AST::Var->new(
                    sigil       => $sigil,
                    namespace   => Perlito5::Match::flat($m),
                    name        => Perlito5::Match::flat($n),
                );
                $m->{to} = $spc->{to};
                # hijack some string interpolation code to parse the subscript
                $m = Perlito5::Grammar::String->double_quoted_var_with_subscript($m);
                $m->{capture} = [ 'term', $m->{capture} ];
                $spc = Perlito5::Grammar::Space->opt_ws($str, $m->{to});
                my $p = $spc->{to};
                if ( substr($str, $p, 1) eq '}' ) {
                    $m->{to} = $p + 1;
                    return $m;
                }
            }
        }
        if ( substr($str, $p, 1) eq '^' ) {
            #  ${^ ...
            # TODO - make sure ^ is followed by an ASCII uppercase letter
            $m = Perlito5::Grammar->var_name( $str, $p + 1 );
            if ($m) {
                my $p = $m->{to};
                if ( substr($str, $p, 1) eq '}' ) {
                    my $name = Perlito5::Match::flat($m);
                    my $c1 = chr( ord(substr($name, 0, 1)) - ord("A") + 1 );
                    $m->{capture} = [ 'term', 
                            Perlito5::AST::Apply->new(
                                'arguments' => [
                                    Perlito5::AST::Val::Buf->new(
                                        'buf' => $c1 . substr($name, 1),
                                    )
                                ],
                                'code' => 'prefix:<' . $sigil . '>',
                                'namespace' => '',
                            )
                        ];
                    $m->{to} = $m->{to} + 1;
                    return $m;
                }
            }
        }
        $m = Perlito5::Expression->curly_parse( $str, $p );
        if ($m) {
            #  ${ ... }
            my $p = $m->{to};
            if ( substr($str, $p, 1) eq '}' ) {
                $m->{to} = $m->{to} + 1;
                $m->{capture} = [ 'term',  
                        Perlito5::AST::Apply->new( 
                                'arguments' => [ $m->{capture} ],
                                'code'      => 'prefix:<' . $sigil . '>', 
                                'namespace' => ''
                            )
                    ];
                return $m;
            }
        }
    }
    if ( $c1 eq '^' ) {
        #  $^ ...
        # TODO - make sure ^ is followed by an ASCII uppercase letter
        my $p = $q;
        $m = Perlito5::Grammar->word( $str, $p );
        if ($m) {
            my $name = Perlito5::Match::flat($m);
            my $c1 = chr( ord(substr($name, 0, 1)) - ord("A") + 1 );
            $m->{capture} = [ 'term',  
                        Perlito5::AST::Apply->new(
                            'arguments' => [
                                Perlito5::AST::Val::Buf->new(
                                    'buf' => $c1 . substr($name, 1),
                                )
                            ],
                            'code' => 'prefix:<' . $sigil . '>',
                            'namespace' => '',
                        )
                    ];
            return $m;
        }
    }
    if ( $c1 eq '$' ) {
        #  $$ ...
        $m = $self->term_sigil( $str, $p );
        if ($m) {
            $m->{capture} = [ 'term',  
                    Perlito5::AST::Apply->new( 
                            'arguments' => [ $m->{capture}[1] ],
                            'code'      => 'prefix:<' . $sigil . '>', 
                            'namespace' => ''
                        )
                ];
            return $m;
        }
    }

    $m = Perlito5::Grammar->optional_namespace_before_ident( $str, $p );
    if ($m) {
        #  $name ...
        my $n = Perlito5::Grammar->var_name( $str, $m->{to} );
        if ($n) {
            $n->{capture} = [ 'term', 
                    Perlito5::AST::Var->new(
                            sigil       => $sigil,
                            namespace   => Perlito5::Match::flat($m),
                            name        => Perlito5::Match::flat($n),
                        )
                ];
            return $n;
        }
    }

    #  $! ...
    return $self->term_special_var( $str, $pos );
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

