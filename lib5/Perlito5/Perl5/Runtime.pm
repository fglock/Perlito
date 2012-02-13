
use v5;

use Perlito5::Match;

package Perlito5::Grammar;
    sub space {
        # my $grammar = $_[0];
        my $str = $_[1]; my $pos = $_[2];
        my $MATCH = bless { str => $str, from => $pos, to => $pos }, 'Perlito5::Match';
        $MATCH->{bool} = (
            substr($str, $MATCH->{to}) =~ m/^([[:space:]])/
            ? ( 1 + ($MATCH->{to} = ( length( $1 ) + $MATCH->{to} )))
            : 0
        );
        $MATCH;
    }
    sub digit {
        # my $grammar = $_[0];
        my $str = $_[1]; my $pos = $_[2];
        my $MATCH = bless { str => $str, from => $pos, to => $pos }, 'Perlito5::Match';
        $MATCH->{bool} = (
            substr($str, $MATCH->{to}) =~ m/^([[:digit:]])/
            ? ( 1 + ($MATCH->{to} = ( length( $1 ) + $MATCH->{to} )))
            : 0
        );
        $MATCH;
    }

    sub word {
        # my $grammar = $_[0];
        my $str = $_[1]; my $pos = $_[2];
        my $MATCH = bless { str => $str, from => $pos, to => $pos }, 'Perlito5::Match';
        $MATCH->{bool} = (
            substr($str, $MATCH->{to}) =~ m/^([[:word:]])/
            ? ( 1 + ($MATCH->{to} = ( length( $1 ) + $MATCH->{to} )))
            : 0
        );
        $MATCH;
    }

package IO;

    sub slurp {
        my $source_filename = shift;
        open FILE, $source_filename
          or die "Cannot read $source_filename\n";
        local $/ = undef;
        $source = <FILE>;
        close FILE;
        return $source;
    }

package Perlito5::Runtime;

    sub map  { bless [ CORE::map(  $_[0]($_), @{$_[1]} ) ], 'ARRAY' }
    sub grep { bless [ CORE::grep( $_[0]($_), @{$_[1]} ) ], 'ARRAY' }
    sub sort {
          $_[1]
        ? bless [ CORE::sort( $_[0]($_), @{$_[1]} ) ], 'ARRAY'
        : bless [ CORE::sort( @{$_[0]} ) ], 'ARRAY'
    }

    sub Str {
        my $can = UNIVERSAL::can($o => 'Str');
        return $can->($o) if $can;
        if ( ref($_[0]) ) {
            return ARRAY::Str($_[0]) if ref($_[0]) eq 'ARRAY';
            return HASH::Str($_[0])  if ref($_[0]) eq 'HASH';
        }
        return $_[0];
    }
    sub say   { print( @_, "\n" ) }

    # XXX Perl6
    sub perl {
        return 'undef' unless defined $_[0];
        local $_;
        local %Perlito5::Runtime::_seen = %Perlito5::Runtime::_seen;
        my $o = shift;
        if ( ref($o) ) {
            my $key = "$o";
            return "'!!! Recursive structure !!!' at $key" if ($Perlito5::Runtime::_seen{$key} || 0) > 3;
            $Perlito5::Runtime::_seen{$key}++;
            return '[' . join( ", ", map { perl($_) } @$o ) . ']'
                if ref($o) eq 'ARRAY';
            return '{' . join( ", ", map { perl($_) . ' => ' . perl($o->{$_}) } sort {$a cmp $b} CORE::keys(%$o) ) . '}'
                if ref($o) eq 'HASH';
            return 'sub { ... }'
                if ref($o) eq 'CODE';
        }
        else {
            return $o if $o =~ /^[0-9]/ && (0+$o) eq $o;
            $o =~ s/\\/\\\\/g;
            $o =~ s/'/\\'/g;
            return "'" . $o . "'";
        }
        my $ref = ref($o);
        return perl($$o) if $ref eq 'SCALAR';
        return $ref . "->new("
            . join( ", ", map { Perlito5::Runtime::perl($_) . ' => ' . Perlito5::Runtime::perl($o->{$_}) } sort {$a cmp $b} CORE::keys(%$o) )
            . ")";
    }

    sub split {
        return '' unless defined $_[0];
        my $can = UNIVERSAL::can($_[0] => 'split');
        if ($can) {
            $can->(@_);
        }
        else {
            [ split($_[1], $_[0], -1) ];
        }
    }

    sub bool {
        $_[0] ? 1 : 0
    }

1;

__END__

=pod

=head1 NAME

Perlito5::Perl5::Runtime

=head1 DESCRIPTION

Provides runtime routines for the Perlito-in-Perl5 compiled code

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
