use v5;

package Perlito5::Grammar::Sigil;

use Perlito5::Grammar::Precedence;


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
    '$]'  => '',
    '$}'  => '',
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
    my $str = $_[0];
    my $pos = $_[1];
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

    my $s = $str->[$pos] . $str->[$pos + 1] . $str->[$pos + 2];
    if ( $s eq '$#[' ) {
        # special case: $# is not valid, but @# is ok
        $len = 2;
    }
    elsif ( exists $special_var{$s} ) {
        $len = length($s);
    }
    else {
        $s = $str->[$pos] . $str->[$pos + 1];
        if ( exists $special_var{$s} ) {
            $len = 2;
        }
    }
    if ( $len ) {
        my $c0 = $str->[ $pos + $len - 1 ];
        my $c1 = $str->[ $pos + $len ];
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
                                        name        => substr($s, $len - 1, 1),
                                        _namespace  => 'main',
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
    my $str = $_[0];
    my $pos = $_[1];

    my $c1 = $str->[$pos];
    return unless exists $sigil{$c1};

    my $p = $pos + 1;
    my $sigil = $c1;
    if ( $c1 eq '$' && $str->[$pos+1] eq '#') {
        $sigil = '$#';
        $p++;
    }

    my $m = Perlito5::Grammar::Space::opt_ws($str, $p);
    $p = $m->{to};

    my $p0 = $p;
    $c1 = $str->[$p];
    my $q = $p + 1;
    if ( $c1 eq '{' ) {
        #  ${ ...
        my $p = $q;
        $m = Perlito5::Grammar::Space::opt_ws($str, $p);
        $p = $m->{to};

        $m = Perlito5::Grammar::optional_namespace_before_ident( $str, $p );
        if ($m) {
            my $namespace = Perlito5::Match::flat($m);
            my $pos  = $m->{to};
            #  ${name  ...
            my $n = Perlito5::Grammar::var_name( $str, $m->{to} );
            my $name;
            if ($n) {
                $name = Perlito5::Match::flat($n);
                $pos  = $n->{to};
            }
            if ($namespace || $name) {
                my $spc = Perlito5::Grammar::Space::opt_ws($str, $pos);
                my $pos = $spc->{to};
                if ($str->[$pos] eq '{' || $str->[$pos] eq '[' || $str->[$pos] eq '}') {
                    # we are not parsing:  ${subr()}
                    # we are parsing:  ${var}  ${var{index}}  ${var[index]}
                    # create the 'Var' object
                    $m->{capture} = Perlito5::AST::Var->new(
                        sigil       => $sigil,
                        namespace   => $namespace,
                        name        => $name,
                    );
                    $m->{to} = $spc->{to};
                    # hijack some string interpolation code to parse the subscript
                    $m = Perlito5::Grammar::String::double_quoted_var_with_subscript($m);
                    $m->{capture} = [ 'term', $m->{capture} ];
                    $spc = Perlito5::Grammar::Space::opt_ws($str, $m->{to});
                    my $p = $spc->{to};
                    if ( $str->[$p] eq '}' ) {
                        $m->{to} = $p + 1;
                        return $m;
                    }
                }
                elsif ($str->[$pos] eq '-' && $str->[$pos + 1] eq '>') {
                    my $spc = Perlito5::Grammar::Space::opt_ws($str, $pos + 2);
                    my $pos = $spc->{to};
                    if ($str->[$pos] eq '{' || $str->[$pos] eq '[') {
                        # we are parsing:  ${var->{index}}  ${var->[index]}
                        # create the 'Var' object
                        $m->{capture} = Perlito5::AST::Var->new(
                            sigil       => $sigil,
                            namespace   => ( $namespace || $Perlito5::PKG_NAME ),  # deref variable is always global
                            name        => $name,
                        );
                        $m->{to} = $spc->{to};
                        # hijack some string interpolation code to parse the subscript
                        $m = Perlito5::Grammar::String::double_quoted_var_with_subscript($m);
                        $m->{capture} = [ 'term', $m->{capture} ];
                        $spc = Perlito5::Grammar::Space::opt_ws($str, $m->{to});
                        my $p = $spc->{to};
                        if ( $str->[$p] eq '}' ) {
                            $m->{to} = $p + 1;
                            return $m;
                        }
                    }
                }
            }
        }
        my $caret = Perlito5::Grammar::caret_char( $str, $p );
        if ( $caret ) {
            #  ${^ ...
            my $p = $caret->{to};
            my $name = Perlito5::Match::flat($caret);
            $m = Perlito5::Grammar::var_name($str, $p);
            if ($m) {
                $name = $name . Perlito5::Match::flat($m);
                $p = $m->{to};
            }
            if ( $str->[$p] eq '}' ) {
                $caret->{capture} = [ 'term', 
                        Perlito5::AST::Var->new(
                            name => $name,
                            namespace => '',
                            sigil => $sigil,
                        ),
                    ];
                $caret->{to} = $p + 1;
                return $caret;
            }
        }
        my $special = $sigil . $str->[$p];
        if ( exists $special_var{$special} ) {
            # ${@}  $#{+}  - special variable
            my $m = Perlito5::Grammar::Space::opt_ws($str, $p + 1);
            my $p2 = $m->{to};
            my $c2 = $str->[$p2];
            if ($c2 eq '}') {
                $m->{to} = $p2 + 1;
                $m->{capture} = [ 'term', 
                        Perlito5::AST::Var->new(
                                sigil       => $sigil,
                                namespace   => '',
                                name        => $str->[$p],
                                ( $sigil eq '$#' ? ( _real_sigil => '@' ) : () ),
                                _namespace  => 'main',
                            )
                    ];
                return $m;
            }
        }
        if ($str->[$p] eq '}') {
            # ${}
            Perlito5::Compiler::error( "syntax error" );
        }
        $m = Perlito5::Grammar::block( $str, $p0 );
        if ($m) {
            #  ${ ... }
            my $ast = Perlito5::Match::flat($m);

            if (@{$ast->{stmts}} == 1
               && (  ref($ast->{stmts}[0]) eq 'Perlito5::AST::Apply'
                  || ref($ast->{stmts}[0]) eq 'Perlito5::AST::Call' 
                  || ref($ast->{stmts}[0]) eq 'Perlito5::AST::Var' 
                  || ref($ast->{stmts}[0]) eq 'Perlito5::AST::Buf' 
                  || ref($ast->{stmts}[0]) eq 'Perlito5::AST::Index' 
                  || ref($ast->{stmts}[0]) eq 'Perlito5::AST::Lookup' )
            ) {
                $m->{capture} = [
                    'term',
                    Perlito5::AST::Apply->new(
                        code      => 'prefix:<' . $sigil . '>',
                        namespace => '',
                        arguments => [ $ast->{stmts}[0] ],
                        _strict_refs => ( $^H & $Perlito5::STRICT_REFS ),
                    ),
                ];
                return $m;
            }
            
            $m->{capture} = [
                'term',
                Perlito5::AST::Apply->new(
                    code      => 'prefix:<' . $sigil . '>',
                    arguments => [
                        Perlito5::AST::Apply->new(
                            code      => 'do',
                            namespace => '',
                            arguments => [ $ast ],
                        )
                    ],
                    _strict_refs => ( $^H & $Perlito5::STRICT_REFS ),
                )
            ];
            return $m;
        }
    }
    my $caret = Perlito5::Grammar::caret_char( $str, $p );
    if ( $caret ) {
        #  $^ ...
        my $name = Perlito5::Match::flat($caret);
        $caret->{capture} = [ 'term',  
                    Perlito5::AST::Var->new(
                        name => $name,
                        namespace => '',
                        sigil => $sigil,
                    ),
                ];
        return $caret;
    }
    if ( $c1 eq '$' ) {
        #  $$ ...
        my $m2 = Perlito5::Grammar::Space::opt_ws($str, $p + 1);
        my $p2 = $m2->{to};
        my $c2 = $str->[$p2];
        if ($c2 ne ',' && $c2 ne ';') {
            # not $$; not $$,
            $m = term_sigil( $str, $p );
            if ($m) {
                $m->{capture} = [ 'term',  
                        Perlito5::AST::Apply->new( 
                                arguments  => [ $m->{capture}[1] ],
                                code       => 'prefix:<' . $sigil . '>', 
                                namespace  => '',
                                _strict_refs => ( $^H & $Perlito5::STRICT_REFS ),
                            )
                    ];
                return $m;
            }
        }
    }

    $m = Perlito5::Grammar::optional_namespace_before_ident( $str, $p );
    if ($m) {
        my $namespace = Perlito5::Match::flat($m);
        #  $name ...
        my $n = Perlito5::Grammar::var_name( $str, $m->{to} );
        if ($n) {
            $n->{capture} = [ 'term', 
                    Perlito5::AST::Var->new(
                            sigil       => $sigil,
                            namespace   => $namespace,
                            name        => Perlito5::Match::flat($n),
                            ( $sigil eq '$#' ? ( _real_sigil => '@' ) : () ),
                        )
                ];
            return $n;
        }
        if ($namespace) {
            $m->{capture} = [ 'term', 
                    Perlito5::AST::Var->new(
                            sigil       => $sigil,
                            namespace   => $namespace,
                            name        => undef,
                            ( $sigil eq '$#' ? ( _real_sigil => '@' ) : () ),
                        )
                ];
            return $m;
        }

    }

    #  $! ...
    return term_special_var( $str, $pos );
};


Perlito5::Grammar::Precedence::add_term( '$'  => \&term_sigil );
Perlito5::Grammar::Precedence::add_term( '@'  => \&term_sigil );
Perlito5::Grammar::Precedence::add_term( '%'  => \&term_sigil );
Perlito5::Grammar::Precedence::add_term( '&'  => \&term_sigil );
Perlito5::Grammar::Precedence::add_term( '*'  => \&term_sigil );


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

