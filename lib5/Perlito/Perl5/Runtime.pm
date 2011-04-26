
use v5;
binmode(STDOUT, ":utf8");
use Scalar::Util;
use Encode;

$_ = Encode::decode('utf-8', $_)
    for @ARGV;

{
    package Perlito::Match;
    
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
    sub pairs {
        map Pair->new( key => $_, value => $_[0]{$_} ),
            CORE::keys %{$_[0]}
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

package Perlito::Grammar;
    sub space { 
        my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2]; 
        my $MATCH; 
        $MATCH = Perlito::Match->new( 
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
        my $MATCH; $MATCH = Perlito::Match->new( 
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
        my $MATCH; $MATCH = Perlito::Match->new( 
            str => $str,from => $pos,to => $pos, ); 
        $MATCH->{bool} = (
            substr($str, $MATCH->to()) =~ m/^([[:word:]])/
            ? ( 1 + ($MATCH->{to} = ( length( $1 ) + $MATCH->to() )))
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
        return Encode::decode( 'utf-8', $source );
    }

package Main;

    sub True { 1 }
    sub Str {
        local $_;
        if ( ref($_[0]) ) {
            return join( " ", map { Str($_) } @{$_[0]} ) if ref($_[0]) eq 'ARRAY';
            return join( "\n", map { $_ . "\t" . Str($_[0]{$_}) } keys %{$_[0]} ) if ref($_[0]) eq 'HASH';
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
        return 1;
    }
    sub say   { Main::print( @_, "\n" ) }
    sub chars { length( $_[0] ) }
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

    sub pairs {
        [
            map Pair->new( key => $_, value => $_[0]{$_} ),
                CORE::keys %{$_[0]}
        ]
    }
 
    sub id {
           Scalar::Util::refaddr($_[0]) 
        || "_id_" . $_[0]
    }

    sub perl {
        return 'Mu' unless defined $_[0];
        local $_;
        local %Main::_seen = %Main::_seen;
        my $o = shift;
        if ( ref($o) ) {
            my $key = "$o";
            return "'!!! Recursive structure !!!' at $key" if ($Main::_seen{$key} || 0) > 3;
            $Main::_seen{$key}++;
            return '[' . join( ", ", map { perl($_) } @$o ) . ']' 
                if ref($o) eq 'ARRAY';
            return '{' . join( ", ", map { perl($_) . ' => ' . perl($o->{$_}) } sort {$a cmp $b} keys(%$o) ) . '}' 
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
        my $ref = ref($_[0]);
        return scalar(@{$_[0]}) if $ref eq 'ARRAY';
        return scalar(keys %{$_[0]}) if $ref eq 'HASH';
        return $_[0] ? 1 : 0;
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
    # Javascript emitter
    sub to_javascript_namespace {
        my $s = $_[0];
        my $sigil;
        ( $sigil, $s ) = $s =~ /^([$@%]?)(.*)$/;
        $s =~ s/::/\$/g;
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

Perlito::Perl5::Runtime

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
