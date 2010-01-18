package MiniPerl6::Match;
# Documentation in the __END__

use 5.006;
use strict;
use warnings;
no warnings 'recursion';
use Data::Dumper;
#use Class::InsideOut qw( public register id );
use Scalar::Util qw( refaddr blessed );

my %_data;

use overload (
    '@{}'    => \&array,
    '%{}'    => \&hash,
    bool   => sub { $_data{refaddr $_[0]}{bool} },
    '&{}'    => \&code,
    '${}'    => \&scalar,
    '""'     => \&flat,
    '0+'     => \&flat,
    fallback => 1,
);

# class method
# ::fail can be called from inside closures
# sub ::fail { $::_V6_SUCCEED = 0 }

sub new {
    my $class = shift;
    my $obj = bless \$class, $class;
    #print "Match->new( @_ ) ",(refaddr $obj),"\n";
    $_data{ refaddr $obj } = { @_ };
    return $obj;
}

sub DESTROY {  
    delete $_data{ refaddr $_[0] };
}

sub data  {    $_data{refaddr $_[0]}           }

#sub from  {    $_data{refaddr $_[0]}->{from}   }
#sub to    {    $_data{refaddr $_[0]}->{to}     }
#sub bool  {    $_data{refaddr $_[0]}->{bool}   }

sub from :lvalue { @_ == 1 ? ( $_data{refaddr $_[0]}{from} ) : ( $_data{refaddr $_[0]}{from} = $_[1] ) };
sub to   :lvalue { @_ == 1 ? ( $_data{refaddr $_[0]}{to}   ) : ( $_data{refaddr $_[0]}{to}   = $_[1] ) };
sub bool :lvalue { @_ == 1 ? ( $_data{refaddr $_[0]}{bool} ) : ( $_data{refaddr $_[0]}{bool} = $_[1] ) };
sub capture :lvalue
         { @_ == 1 ? ( $_data{refaddr $_[0]}{capture} ) : ( $_data{refaddr $_[0]}{capture} = $_[1] ) };

sub array {    
         $_data{refaddr $_[0]}->{match} 
    || ( $_data{refaddr $_[0]}->{match} = [] )
}

sub hash  {   
         $_data{refaddr $_[0]}->{named} 
    || ( $_data{refaddr $_[0]}->{named} = {} )

# XXX - doesn't work as lvalue
#    my $array = 
#             $_data{refaddr $_[0]}->{match} 
#        || ( $_data{refaddr $_[0]}->{match} = [] );
#    return {
#        %{ $_data{refaddr $_[0]}->{named} || {} },
#        (
#        map { ( $_, $array->[$_] ) } 
#            0 .. $#$array
#        ),
#    }
}

sub keys   { 
    CORE::keys   %{$_data{refaddr $_[0]}->{named}},
    0 .. $#{ $_[0]->array }
}
sub values { 
    CORE::values %{$_data{refaddr $_[0]}->{named}},
    @{ $_[0]->array }
}
sub kv {
    map { ( $_, $_[0]->{$_} ) } 
        $_[0]->keys 
}
sub elems  { 
    scalar $_[0]->keys
}

sub chars  { CORE::length $_[0]->str }

sub flat {
    my $obj = $_data{refaddr $_[0]};
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

sub perl {
    local $Data::Dumper::Terse    = 1;
    local $Data::Dumper::Sortkeys = 1;
    local $Data::Dumper::Pad = '  ';
    return __PACKAGE__ . "->new( " . Dumper( $_[0]->data ) . ")\n";
}

sub yaml {
    require YAML::Syck;
    # interoperability with other YAML/Syck bindings:
    $YAML::Syck::ImplicitTyping = 1;
    YAML::Syck::Dump( $_[0] );
}

# for Pugs interoperability
sub dump_hs {
    my $obj;
    if (ref($_[0]) eq 'SCALAR') {
        $obj = ${$_[0]};
    }
    else {
        $obj = $_data{refaddr $_[0]};
    }

    if ($obj) {
        # Ok, this is a genuine Match object.
        return "PGE_Fail" unless ${$obj->{bool}};

        # Now we matched; dump the rest of data
        join(' ', 'PGE_Match', ${$obj->{from}}, ${$obj->{to}},
            ('['.join(', ', map { dump_hs($_) } @{$obj->{match}||[]} ).']'),
            ('['.join(', ', map {
                my $str = $_;
                if ( my $dump = dump_hs($obj->{named}{$_}) ) {
                    $str =~ s/([^ \!\#\$\%\&\x28-\x5B\x5D-\x7E])/'\\'.ord($1)/eg;
                    qq[("$str", $dump)];
                }
                else {
                    ();
                }
            } sort(CORE::keys(%{$obj->{named}||{}})) ).']'),
        )
    }
    elsif (ref($_[0]) eq 'ARRAY') {
        return "PGE_Array [" . join(', ', map { dump_hs($_) } @$obj) . "]"
    }
    elsif (!ref($_[0])) {
        my $str = shift;
        $str =~ s/([^ \!\#\$\%\&\x28-\x5B\x5D-\x7E])/'\\'.ord($1)/eg;
        return "PGE_String \"$str\"";
    }
    else {
        warn "Unrecognized blessed match object: $_[0]";
        return '';
    }
}

# tail() for backwards compatibility
# - doesn't work on failed matches
sub tail {
    return substr( ${$_data{refaddr $_[0]}->{str}}, $_[0]->to );
}

# state() is used for multiple matches and backtracking control
sub state {
    return $_data{refaddr $_[0]}->{state};
}

# return the capture
sub code {
    my $c = $_[0];
    return sub { $c->flat };
}

# return the capture
sub scalar {
    return \( $_[0]->flat );
}

1;

__END__

=head1 NAME 

MiniPerl6::Match - Match object created by rules

=head1 METHODS

* array

- return the positional matches

* hash

- return both the named and positional (numbered) matches

* str

- return the stringified capture object. 
If there is no capture, return the matched substring

* scalar

- return the capture object
If there is no capture, return the matched substring

* bool

- return whether there was a match

* from

- return the string position where the match started

* to

- return the string position immediately after where the match finished

=head1 "Hash" methods

* elems

* kv

* keys

* values

=head1 "Str" methods

* chars

=head1 OVERLOADS

* $match->()

- return the capture object

* $match->[$n]

- return the positional matches

* $match->{$n}

- return the named matches

* $match ? 1 : 0

- return whether there was a match

=head1 Dumper methods

* data

- return the internal representation as a data structure.

* perl

- return the internal representation as Perl source code. 

* yaml

- return the internal representation as YAML. 
Requires the C<YAML::Syck> module.

* dump_hs

- for Pugs interoperability

=head1 SEE ALSO

C<v6> on CPAN

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2006, 2009 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut

