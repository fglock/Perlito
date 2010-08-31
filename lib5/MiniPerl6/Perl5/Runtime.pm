
use v5;
binmode(STDOUT, ":utf8");

{
    package MiniPerl6::Match;
    
    use strict;
    use warnings;
    no warnings 'recursion';
    
    use overload (
        '@{}'    => \&array,
        bool     => \&bool,
        '${}'    => \&scalar,
        '""'     => \&flat,
        '0+'     => \&flat,
        'eq'     => sub { "$_[0]" eq "$_[1]" },
    );
    
    sub new {
        my ($class, %data) = @_;
        return bless \%data, $class;
    }
    
    sub from { $_[0]{from} }
    sub to   { $_[0]{to} }
    sub bool { $_[0]{bool} }
    sub capture { $_[0]{capture} }
    
    sub array {    
        my $v = $_[0];
             $v->{match} 
        || ( $v->{match} = [] )
    }
    
    sub hash  {   
        $_[0]
    }
    
    sub keys   { 
        CORE::keys %{$_[0]};
    }
    sub values { 
        CORE::values %{$_[0]};
    }
    
    sub flat {
        my $obj = $_[0];
        my $cap = $obj->{capture};
        #print ref $cap;
        return $cap
            if defined $cap;
        return '' unless $obj->{bool};
        return '' if $_[0]->from > length( $obj->{str} );
        return substr( $obj->{str}, $_[0]->from, $_[0]->to - $_[0]->from );
    }
    
    sub str {
        "" . $_[0]->flat;
    }
    
    sub scalar {
        return \( $_[0]->flat );
    }
    
}

package MiniPerl6::Grammar;
    sub space { 
        my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2]; 
        my $MATCH; 
        $MATCH = MiniPerl6::Match->new( 
            str => $str,from => $pos,to => $pos, ); 
        $MATCH->{bool} = (
            substr($str, $MATCH->to()) =~ m/^([[:space:]])/
            ? ( 1 + ($MATCH->{to} = ( length( $1 ) + $MATCH->to() )))
            : 0
        );
        $MATCH;
    }
    sub digit { 
        my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2]; 
        my $MATCH; $MATCH = MiniPerl6::Match->new( 
            str => $str,from => $pos,to => $pos, ); 
        $MATCH->{bool} = (
            substr($str, $MATCH->to()) =~ m/^([[:digit:]])/
            ? ( 1 + ($MATCH->{to} = ( length( $1 ) + $MATCH->to() )))
            : 0
        );
        $MATCH;
    }

    sub word { 
        my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2]; 
        my $MATCH; $MATCH = MiniPerl6::Match->new( 
            str => $str,from => $pos,to => $pos, ); 
        $MATCH->{bool} = (
            substr($str, $MATCH->to()) =~ m/^([[:word:]])/
            ? ( 1 + ($MATCH->{to} = ( length( $1 ) + $MATCH->to() )))
            : 0
        );
        $MATCH;
    }

    sub is_newline { 
        my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2]; 
        my $MATCH; $MATCH = MiniPerl6::Match->new( 
            str => $str,from => $pos,to => $pos, ); 
        return $MATCH unless ord( substr($str, $MATCH->to()) ) == 10
            || ord( substr($str, $MATCH->to()) ) == 13;
        $MATCH->{bool} = (
            substr($str, $MATCH->to()) =~ m/(?m)^(\n\r?|\r\n?)/
            ? ( 1 + ($MATCH->{to} = ( length( $1 ) + $MATCH->to() )))
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
        $MATCH->{to} = ( 1 + $MATCH->to );
        $MATCH->{bool} = ( 1 );
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

    sub True { 1 }
    sub Str {
        local $_;
        if ( ref($_[0]) ) {
            return join( " ", map { Str($_) } @{$_[0]} ) if ref($_[0]) eq 'ARRAY';
        }
        return $_[0];
    }
    sub print { 
        local $_;
        for (@_) {
            if ( ref($_) ) {
                CORE::print Main::Str($_);
                next;
            }
            CORE::print $_
        } 
    }
    sub say   { Main::print( @_, "\n" ) }
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
        return 'Mu' unless defined $_[0];
        local $_;
        local %Main::_seen = %Main::_seen;
        my $o = shift;
        if ( ref($o) ) {
            my $key = "$o";
            return "'!!! Recursive structure !!!' at $key" if $Main::_seen{$key} > 3;
            $Main::_seen{$key}++;
            return '[' . join( ", ", map { perl($_) } @$o ) . ']' 
                if ref($o) eq 'ARRAY';
            return '{' . join( ", ", map { perl($_) . ' => ' . perl($o->{$_}) } sort {$a cmp $b} keys(%$o) ) . '}' 
                if ref($o) eq 'HASH';
            return 'sub { ... }'
                if ref($o) eq 'CODE';
        }
        else {
            return $o if (0+$o) eq $o;
            return "'" . perl_escape_string($o) . "'";
        }
        my $can = UNIVERSAL::can($o => 'perl');
        return $can->($o) if $can;
        my $ref = ref($o);
        return perl($$o) if $ref eq 'SCALAR';
        return $ref . ".new("
            . join( ", ", map { Main::perl($_) . ' => ' . Main::perl($o->{$_}) } sort {$a cmp $b} CORE::keys(%$o) )
            . ")";
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
        return '' unless defined $_[0];
        my $can = UNIVERSAL::can($_[0] => 'join');
        if ($can) {
            $can->(@_);
        }
        else {
            join($_[1], @{$_[0]} );
        }
    }

    sub bool { 
        my $ref = ref($_[0]);
        return scalar(@{$_[0]}) if $ref eq 'ARRAY';
        return $_[0];
    }

    # Lisp emitter
    sub to_lisp_identifier {
        my $s = $_[0];
        my $sigil;
        ( $sigil, $s ) = $s =~ /^([$@%]?)(.*)$/;
        return 'sv-' . $s;
    }
    sub to_lisp_namespace {
        my $s = $_[0];
        my $sigil;
        ( $sigil, $s ) = $s =~ /^([$@%]?)(.*)$/;
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
        my $sigil;
        ( $sigil, $s ) = $s =~ /^([$@%]?)(.*)$/;
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
        my $sigil;
        ( $sigil, $s ) = $s =~ /^([$@%]?)(.*)$/;
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
