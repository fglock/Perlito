
package Perlito5::Grammar::Bareword;
use strict;


token the_object {
        <before '$'> <Perlito5::Grammar::Sigil.term_sigil>
            {
                $MATCH->{capture} = Perlito5::Match::flat($MATCH->{'Perlito5::Grammar::Sigil.term_sigil'})->[1];
            }
    |
        '{' <Perlito5::Expression.curly_parse> '}'
            {
                $MATCH->{capture} = Perlito5::Match::flat($MATCH->{'Perlito5::Expression.curly_parse'});
            }
    |
        <Perlito5::Grammar::Print.typeglob>
            {
                $MATCH->{capture} = Perlito5::Match::flat($MATCH->{'Perlito5::Grammar::Print.typeglob'});
            }
};

    sub term_bareword {
        my $self = $_[0];
        my $str = $_[1];
        my $pos = $_[2];

        my $p = $pos;
        my $m_namespace = Perlito5::Grammar->optional_namespace_before_ident( $str, $p );
        my $namespace = Perlito5::Match::flat($m_namespace);
        $p = $m_namespace->{to};
        my $m_name      = Perlito5::Grammar->ident( $str, $p );

        if (!$m_name) {
            if ($namespace) {
                # namespace without name - X::
                $m_namespace->{capture} = [ 'term', 
                            Perlito5::AST::Var->new(
                                sigil => '::',
                                name  => '',
                                namespace => $namespace,
                            )
                        ];
                return $m_namespace;
            }
            return;
        }

        my $name = Perlito5::Match::flat($m_name);
        $p = $m_name->{to};

        if ( substr( $str, $p, 2) eq '::' ) {
            # ::X::y::
            $m_name->{to} = $p + 2;
            $m_name->{capture} = [ 'term', 
                        Perlito5::AST::Var->new(
                            sigil => '::',
                            name  => '',
                            namespace => $namespace . '::' . $name,
                        )
                    ];
            return $m_name;
        }

        my $full_name = $name;
        $full_name = $namespace . '::' . $name if $namespace;

        # we've got a bareword

        # my $has_space_after;
        my $m = Perlito5::Grammar::Space->ws( $str, $p );
        if ( $m ) {
            # $has_space_after = 1;
            $p = $m->{to};
        }

        # check for indirect-object
        my $invocant;
        my $is_subroutine_name;
        my $effective_name = ( $namespace || $Perlito5::PKG_NAME ) . '::' . $name;
        if ( exists( $Perlito5::Grammar::Print::Print{$name} ) ) {
            $invocant = undef;
        }
        elsif (  exists( $Perlito5::PROTO->{$effective_name} )       # subroutine was predeclared
               || ( (!$namespace || $namespace eq 'CORE')
                   && exists $Perlito5::CORE_PROTO->{"CORE::$name"}  # subroutine comes from CORE
                 )
               )
        {
            # first term is a subroutine name;
            $is_subroutine_name = 1;

            # this can be an indirect-object if the next term is a bareword ending with '::'

            $invocant = Perlito5::Grammar->full_ident( $str, $p );
            my $package = Perlito5::Match::flat($invocant);
            if ( $package ) {
                $invocant->{capture} = Perlito5::AST::Var->new(
                                         sigil => '::',
                                         name  => '',
                                         namespace => $package,
                                     );
                if ( substr( $str, $invocant->{to}, 2) eq '::' ) {
                    # ::X::y::
                    $invocant->{to} = $invocant->{to} + 2;
                }
                else {
                    # is this a known package name?
                    if ( ! $Perlito5::PACKAGES->{ $package } ) {
                        # not a known package name
                        $invocant = undef;
                    }
                    else {
                        # valid package name - this is indirect-object
                    }
                }
            }

        }
        else {
            $invocant = Perlito5::Grammar::Bareword->the_object( $str, $p );
        }

        if ($invocant) {
            # indirect-object
            $p = $invocant->{to};

            # read the parameter list
            my $arg = [];
            $m = Perlito5::Grammar::Space->ws( $str, $p );
            $p = $m->{to} if $m;
            if ( substr($str, $p, 2) eq '->' ) {
            }
            elsif ( substr($str, $p, 1) eq '(' ) {
                my $m = Perlito5::Expression->term_paren( $str, $p );
                if ( $m ) {
                    $arg = $m->{capture}[2];
                    $p   = $m->{to};
                    $arg = Perlito5::Expression::expand_list( $arg );
                }
            }
            else {
                my $m = Perlito5::Expression->list_parse( $str, $p );
                if ($m->{capture} ne '*undef*') {
                    $arg = $m->{capture};
                    $p   = $m->{to};
                }
            }
            $m_name->{capture} = [ 
                'term', 
                Perlito5::AST::Call->new(
                    'method'    => $full_name,
                    'invocant'  => Perlito5::Match::flat($invocant), 
                    'arguments' => $arg,
                ),
            ];
            $m_name->{to} = $p;
            return $m_name;
        }

        if ( substr( $str, $p, 2 ) eq '=>' ) {
            # autoquote bareword
            $m_name->{capture} = [ 'term', 
                        Perlito5::AST::Apply->new(
                            code      => $name,
                            namespace => $namespace,
                            arguments => [],
                            bareword  => 1
                        )
                    ];
            $m_name->{to} = $p;
            return $m_name;
        }
        if ( substr( $str, $p, 2 ) eq '->' ) {
            if ( $is_subroutine_name ) {
                # call()->method call
                $m_name->{capture} = [ 'term', 
                                       Perlito5::AST::Apply->new(
                                           'arguments' => [],
                                           'code'      => $name,
                                           'namespace' => $namespace,
                                       )
                                     ];
            }
            else {
                # class->method call
                $m_name->{capture} = [ 'term', Perlito5::AST::Proto->new( name => $full_name ) ];
            }
            $m_name->{to} = $p;
            return $m_name;
        }

        # Note: how does this work: (See: perldoc CORE)
        #   $ perl -e ' use strict; sub print { die "here" }; print "123\n"; '
        #   123
        #   $ perl -e ' use strict; BEGIN { *CORE::GLOBAL::time = sub { die "here" } }; print time . "\n"; '
        #   here at -e line 1.
        #   $ perl -e ' use strict; BEGIN { *CORE::GLOBAL::print = sub { die "here" } }; print "123\n"; '
        #   123
        #   $ perl -e ' use strict; use subs "print"; sub print { die "here" }; print "123\n"; '
        #   123
        #
        #   * if it has a prototype it's overridable (thanks rjbs++ in #p5p)
        #   * however, 'chomp' => undef, but I can override it
        #   * core functions that have undefined prototypes and that are overridable:
        #   chomp chop glob exec system require chomp glob exec glob system
        #
        #   $ perl -e ' use strict; BEGIN { *CORE::GLOBAL::chomp = sub { die "here" } }; print chomp . "\n"; '
        #   here at -e line 1.
        
        #   $ perl -e ' use strict; use Data::Dumper; print Dumper prototype("CORE::time")  '
        #   $VAR1 = '';  -> can be overridden
        #   $ perl -e ' use strict; use Data::Dumper; print Dumper prototype("CORE::print")  '
        #   $VAR1 = undef;  -> can't be overwritten

        #   * list of prototypes in CORE:
        #
        #   $ perldoc -u PerlFunc | head -n300 | perl -ne ' push @x, /C<([^>]+)/g; END { eval { $p{$_} = prototype("CORE::$_") } for @x; use Data::Dumper; print Dumper \%p } '


        my $sig;
        if ( exists $Perlito5::PROTO->{$effective_name} ) {
            # subroutine was predeclared
            $sig = $Perlito5::PROTO->{$effective_name};
        }
        elsif ( (!$namespace || $namespace eq 'CORE')
              && exists $Perlito5::CORE_PROTO->{"CORE::$name"} 
              )
        {
            # TODO - CORE::GLOBAL

            # subroutine comes from CORE

            ## XXX - this breaks perl: "CORE::say is not a keyword"
            ## $namespace = "CORE";

            $effective_name = "CORE::$name";
            $sig = $Perlito5::CORE_PROTO->{$effective_name};
        }
        else {
            # TODO - add error messages if needed
            # Bareword "X" not allowed while "strict subs" in use

            # warn "not found: $effective_name";

            ## # it's just a bareword - we will disambiguate later
            ## $m_name->{capture} = [ 'postfix_or_term', 'funcall_no_params',
            ##         $namespace,
            ##         $name
            ##     ];
            ## return $m_name;

            $sig = undef;
        }

        # TODO - parse the parameter list according to the sig

        # say "calling $effective_name ($sig)";

        my $has_paren = 0;
        if ( defined $sig ) {

            if ( substr($sig, 0, 1) eq ';' ) {
                # argument is optional - shift(), pop()

                if ( substr($str, $p, 2) eq '//' ) {
                    # special case - see test t5/01-perlito/25-syntax-defined-or.t
                    # we don't want '//' to be an argument (match) but an operator (defined-or)
                    # so we return 'bareword'

                    $m_name->{capture} = [ 'term', 
                            Perlito5::AST::Apply->new(
                                code      => $name,
                                namespace => $namespace,
                                arguments => [],
                                bareword  => 1,
                            )
                        ];
                    $m_name->{to} = $p;
                    return $m_name;
                }
            }

            if ( $sig eq '' ) {
                # empty sig - we allow (), but only if it is empty
                if ( substr($str, $p, 1) eq '(' ) {
                    $p++;
                    $has_paren = 1;
                    my $m = Perlito5::Grammar::Space->ws( $str, $p );
                    if ($m) {
                        $p = $m->{to}
                    }
                    if ( substr($str, $p, 1) ne ')' ) {
                        die "syntax error near ", substr( $str, $pos, 10 );
                    }
                    $p++;
                }
                # TODO - "subs with empty protos are candidates for inlining"
                $m_name->{capture} = [ 'term', 
                        Perlito5::AST::Apply->new(
                            code      => $name,
                            namespace => $namespace,
                            arguments => [],
                            bareword  => ($has_paren == 0)
                        )
                    ];
                $m_name->{to} = $p;
                return $m_name;
            }

            if ( $sig eq '_' || $sig eq '$' || $sig eq ';$' ) {
                my $m;
                my $arg;
                if ( substr($str, $p, 1) eq '(' ) {
                    $m = Perlito5::Expression->term_paren( $str, $p );
                    if ( !$m ) { return $m };
                    $p = $m->{to};
                    $has_paren = 1;
                    $arg = $m->{capture}[2];
                    $arg = Perlito5::Expression::expand_list( $arg );
                    my $v = shift @{ $arg };
                    die "Too many arguments for $name"
                        if @{ $arg };
                    $arg = $v;
                }
                else {
                    $m = Perlito5::Expression->argument_parse( $str, $p );
                    $arg = $m->{capture};
                    if ( $arg eq '*undef*' ) {
                        $arg = undef;
                    }
                    elsif ( ref($arg) eq 'Perlito5::AST::Apply' && $arg->{code} eq 'circumfix:<( )>' ) {
                        my $v = shift @{ $arg->{arguments} };
                        die "Too many arguments for $name"
                            if @{ $arg->{arguments} };
                        $arg = $v;
                    }
                }
                my @args;
                if ( defined $arg ) {
                    push @args, $arg;
                    $has_paren = 1;
                }
                else {
                    die "Not enough arguments for $name"
                        if $sig eq '$';
                    push @args, Perlito5::AST::Var->new(
                                namespace => '',
                                name      => '_',
                                sigil     => '$'
                            )
                        if $sig eq '_';
                    # ';$' --> ignore the missing arg
                }
                $m->{capture} = [ 'term', 
                        Perlito5::AST::Apply->new(
                            code      => $name,
                            namespace => $namespace,
                            arguments => \@args,
                            bareword  => ($has_paren == 0)
                        )
                    ];
                return $m;
            }

            if ( $sig eq '*' ) {
                # TODO

                # stat FILEHANDLE
                # stat EXPR
                # stat DIRHANDLE
                # If EXPR is omitted, it stats $_.

            }

        } # / defined $sig


        # no sig


        # maybe it's a subroutine call

        if ( substr($str, $p, 1) eq '(' ) {
            $m = Perlito5::Expression->term_paren( $str, $p );
            if ( !$m ) { return $m };
            my $arg = $m->{capture}[2];
            $arg = Perlito5::Expression::expand_list( $arg );
            $m->{capture} = [ 'term', 
                    Perlito5::AST::Apply->new(
                        code      => $name,
                        namespace => $namespace,
                        arguments => $arg
                    )
                ];
            return $m;
        }


        my $m_list = Perlito5::Expression->list_parse( $str, $p );
        my $list = $m_list->{capture};
        if ($list ne '*undef*') {
            $m_name->{capture} = [ 'postfix_or_term', 'funcall',
                    $namespace,
                    $name,
                    $list
                ];
            $m_name->{to} = $m_list->{to};
            return $m_name;
        }

        # it's just a bareword - we will disambiguate later
        $m_name->{capture} = [ 'postfix_or_term', 'funcall_no_params',
                $namespace,
                $name
            ];
        return $m_name;
    }

1;

=begin

=head1 NAME

Perlito5::Grammar::Bareword - Parser and AST generator for Perlito

=head1 SYNOPSIS

    term_bareword($str)

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

