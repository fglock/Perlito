
package Perlito5::Grammar::Bareword;
    use Perlito5::Precedence;
    use Perlito5::Grammar;

    sub term_bareword {
        my $self = $_[0];
        my $str = $_[1];
        my $pos = $_[2];

        my $p = $pos;
        my $m_namespace = Perlito5::Grammar->optional_namespace_before_ident( $str, $p );
        $p = $m_namespace->{"to"};
        my $m_name      = Perlito5::Grammar->ident( $str, $p );
        return $m_name
            unless $m_name->{"bool"};
        $p = $m_name->{"to"};

        my $name = $m_name->flat();
        my $namespace = $m_namespace->flat();
        my $full_name = $name;
        $full_name = $namespace . '::' . $name if $namespace;

        # we've got a bareword

        my $has_space_after;
        my $m = Perlito5::Grammar->ws( $str, $p );
        if ( $m->{"bool"} ) {
            $has_space_after = 1;
            $p = $m->{"to"};
        }

        if ( substr( $str, $p, 2 ) eq '=>' ) {
            # autoquote
            $m_name->{"capture"} = [ 'term', Perlito5::AST::Val::Buf->new( buf => $full_name ) ];
            $m_name->{"to"} = $p;
            return $m_name;
        }
        if ( substr( $str, $p, 2 ) eq '->' ) {
            # class-method call
            $m_name->{"capture"} = [ 'term', Perlito5::AST::Proto->new( name => $full_name ) ];
            $m_name->{"to"} = $p;
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


        ## # TODO
        ## my $effective_name = ( $namespace || $Perlito5::PKG_NAME ) . '::' . $name;
        ## if ( exists $Perlito5::PROTO->{$effective_name} ) {
        ##     # subroutine was predeclared
        ##     my $sig = $Perlito5::PROTO->{$effective_name};
        ##     say "calling $effective_name ($sig)";
        ## }
        ## elsif ( exists $Perlito5::CORE_PROTO->{"CORE::$name"} ) {
        ##     # subroutine comes from CORE
        ##     my $sig = $Perlito5::CORE_PROTO->{"CORE::$name"};
        ##     say "calling CORE::$name ($sig)";
        ## }
        ## else {
        ##     say "not found: $effective_name";
        ## }

        if ( $has_space_after ) {
            # maybe it's a subroutine call
            my $m_list = Perlito5::Expression->list_parse( $str, $p );
            if ( $m_list->{"bool"} ) {
                $m_name->{"capture"} = [ 'postfix_or_term', 'funcall',
                        $namespace,
                        $name,
                        $m_list->flat()
                    ];
                $m_name->{"to"} = $m_list->{"to"};
                return $m_name;
            }
        }

        # it's just a bareword - we will disambiguate later
        $m_name->{"capture"} = [ 'postfix_or_term', 'funcall_no_params',
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

