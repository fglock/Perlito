
use v5;

use Data::Dumper;

package MiniPerl6::Grammar;
    use MiniPerl6::Perl5::Match;
    sub space { 
        my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2]; 
        my $MATCH; 
        $MATCH = MiniPerl6::Match->new( 
            str => $str,from => $pos,to => $pos, ); 
        $MATCH->bool(
            substr($str, $MATCH->to()) =~ m/^([[:space:]])/
            ? ( 1 + ($MATCH->to = ( length( $1 ) + $MATCH->to() )))
            : 0
        );
        $MATCH;
    }
    sub digit { 
        my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2]; 
        my $MATCH; $MATCH = MiniPerl6::Match->new( 
            str => $str,from => $pos,to => $pos, ); 
        $MATCH->bool(
            substr($str, $MATCH->to()) =~ m/^([[:digit:]])/
            ? ( 1 + ($MATCH->to = ( length( $1 ) + $MATCH->to() )))
            : 0
        );
        $MATCH;
    }

BEGIN {
    if ( $::_V6_COMPILER_NAME ne 'v6.pm' ) {
        # MP6-in-P5   
        *word = sub { 
            my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2]; 
            my $MATCH; $MATCH = MiniPerl6::Match->new( 
                str => $str,from => $pos,to => $pos, ); 
            $MATCH->bool(
                substr($str, $MATCH->to()) =~ m/^([[:word:]])/
                ? ( 1 + ($MATCH->to = ( length( $1 ) + $MATCH->to() )))
                : 0
            );
            $MATCH;
        };
    }
}

    sub is_newline { 
        my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2]; 
        my $MATCH; $MATCH = MiniPerl6::Match->new( 
            str => $str,from => $pos,to => $pos, ); 
        return $MATCH unless ord( substr($str, $MATCH->to()) ) == 10
            || ord( substr($str, $MATCH->to()) ) == 13;
        $MATCH->bool(
            substr($str, $MATCH->to()) =~ m/(?m)^(\n\r?|\r\n?)/
            ? ( 1 + ($MATCH->to = ( length( $1 ) + $MATCH->to() )))
            : 0
        );
        $MATCH;
    }
    sub not_newline { 
        my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2]; 
        my $MATCH; $MATCH = MiniPerl6::Match->new( 
            str => $str,from => $pos,to => $pos, bool => 0 ); 
        return $MATCH if ord( substr($str, $MATCH->to()) ) == 10
            || ord( substr($str, $MATCH->to()) ) == 13;
        $MATCH->to = ( 1 + $MATCH->to );
        $MATCH->bool( 1 );
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

package Main;

    sub print { print join( '', @_ ) }
    sub say   { print join( '', @_, "\n" ) }
    sub chars { length( $_[0] ) }
    sub newline { "\n" }
    sub quote   { '"' }
    sub isa { 
        my $ref = ref($_[0]);
           (  $ref eq 'ARRAY' 
           && $_[1] eq 'Array'
           )
        || (  $ref eq 'HASH' 
           && $_[1] eq 'Hash'
           )
        || (  $ref eq '' 
           && $_[1] eq 'Str'
           )
        || $ref eq $_[1]
        || (  ref( $_[1] ) 
           && $ref eq ref( $_[1] ) 
           )
    }

    sub perl {
        local $Data::Dumper::Terse    = 1;
        my $can = UNIVERSAL::can($_[0] => 'perl');
        if ($can) {
            $can->($_[0]);
        }
        else {
            Data::Dumper::Dumper($_[0]);
        }
    }
    
    sub yaml {
        my $can = UNIVERSAL::can($_[0] => 'yaml');
        if ($can) {
            $can->($_[0]);
        }
        else {
            require YAML::Syck;
            YAML::Syck::Dump($_[0]);
        }
    }
      
    sub join {
        my $can = UNIVERSAL::can($_[0] => 'join');
        if ($can) {
            $can->(@_);
        }
        else {
            join($_[1], @{$_[0]} );
        }
    }
    
    # Lisp emitter
    sub to_lisp_identifier {
        my $s = $_[0];
        my ( $sigil, $s ) = $s =~ /^([$@%]?)(.*)$/;
        return 'sv-' . $s;
    }
    sub to_lisp_namespace {
        my $s = $_[0];
        my ( $sigil, $s ) = $s =~ /^([$@%]?)(.*)$/;
        $s =~ s/::/-/g;
        return 'mp-' . $s;
    }
    sub lisp_escape_string {
        my $s = $_[0];
        $s =~ s/\\/\\\\/g;
        $s =~ s/"/\\"/g;
        return $s;
    }
    sub javascript_escape_string {
        my $s = $_[0];
        $s =~ s/\\/\\\\/g;
        $s =~ s/"/\\"/g;
        $s =~ s/\n/\\n/g;
        return $s;
    }
    # Javascript emitter
    sub to_javascript_namespace {
        my $s = $_[0];
        my ( $sigil, $s ) = $s =~ /^([$@%]?)(.*)$/;
        $s =~ s/::/\$/g;
        return $s;
    }
    # Perl emitter
    sub perl_escape_string {
        my $s = $_[0];
        $s =~ s/\\/\\\\/g;
        $s =~ s/'/\\'/g;
        return $s;
    }
    # Go emitter
    sub to_go_namespace {
        my $s = $_[0];
        my ( $sigil, $s ) = $s =~ /^([$@%]?)(.*)$/;
        $s =~ s/::/__/g;
        return $s;
    }

1;

__END__

=pod

=head1 NAME 

MiniPerl6::Perl5::Runtime

=head1 DESCRIPTION

Provides runtime routines for the MiniPerl6-in-Perl5 compiled code

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2006, 2009 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
