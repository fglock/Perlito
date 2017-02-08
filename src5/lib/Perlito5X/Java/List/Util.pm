package List::Util;
use strict;

use Exporter qw(import);
our @EXPORT_OK = qw(
    reduce first
    min max minstr maxstr
    concat product sum sum0
    none notall all any
    shuffle
    pairmap
    pairs pairkeys pairvalues
    pairgrep pairfirst
);

package List::Util::_Pair {
    # For objects returned by pairs()
    sub key   { shift->[0] }
    sub value { shift->[1] }
}

sub reduce (&@) {
    if (@_) {
    Java::inline '
        // PlClosure c, PlArray a
        PlObject arg = List__.shift();
        if (arg.is_lvalue()) {
            arg = arg.get();
        }
        PlClosure c = (PlClosure)arg;
        String pkg = c.pkg_name;
        PlObject ret = PlCx.UNDEF;
        int size = List__.to_int();
        if (size == 0) {
            return PlCx.UNDEF;
        }
        if (size == 1) {
            return List__.aget(0);
        }
        PlLvalue v_a_ref = (PlLvalue)PlV.sget(pkg + "::a");
        PlLvalue v_b_ref = (PlLvalue)PlV.sget(pkg + "::b");
        PlObject v_a_val = v_a_ref.get();
        PlObject v_b_val = v_b_ref.get();
        v_a_ref.set(List__.aget(0));
        PlArray empty_args = new PlArray();
        for (int i = 1; i < size; i++) {
            v_b_ref.set(List__.aget(i));
            v_a_ref.set(c.apply(PlCx.SCALAR, empty_args));
        }
        ret = v_a_ref.get();
        v_a_ref.set(v_a_val);
        v_b_ref.set(v_b_val);
        return ret
    ';
    }
    return undef;
}

sub pairgrep (&@) {
    if (@_) {
    Java::inline '
        // PlClosure c, PlArray a
        PlObject arg = List__.shift();
        if (arg.is_lvalue()) {
            arg = arg.get();
        }
        PlClosure c = (PlClosure)arg;
        String pkg = c.pkg_name;
        PlArray ret = new PlArray();
        int size = List__.to_int();
        PlLvalue v_a_ref = (PlLvalue)PlV.sget(pkg + "::a");
        PlLvalue v_b_ref = (PlLvalue)PlV.sget(pkg + "::b");
        PlObject v_a_val = v_a_ref.get();
        PlObject v_b_val = v_b_ref.get();
        PlArray empty_args = new PlArray();
        int i = 0;
        while (i < size) {
            v_a_ref.set(List__.aget(i++));
            v_b_ref.set(List__.aget(i++));
            boolean result = c.apply(PlCx.SCALAR, empty_args).to_boolean();
            if (result) {
                ret.push(new PlArray(v_a_ref, v_b_ref));
            }
        }
        v_a_ref.set(v_a_val);
        v_b_ref.set(v_b_val);
        return (want == PlCx.LIST ) ? ret : ret.length_of_array()
    ';
    }
    return undef;
}

sub pairmap (&@) {
    if (@_) {
    Java::inline '
        // PlClosure c, PlArray a
        PlObject arg = List__.shift();
        if (arg.is_lvalue()) {
            arg = arg.get();
        }
        PlClosure c = (PlClosure)arg;
        String pkg = c.pkg_name;
        PlArray ret = new PlArray();
        int size = List__.to_int();
        PlLvalue v_a_ref = (PlLvalue)PlV.sget(pkg + "::a");
        PlLvalue v_b_ref = (PlLvalue)PlV.sget(pkg + "::b");
        PlObject v_a_val = v_a_ref.get();
        PlObject v_b_val = v_b_ref.get();
        PlArray empty_args = new PlArray();
        int i = 0;
        while (i < size) {
            v_a_ref.set(List__.aget(i++));
            v_b_ref.set(List__.aget(i++));
            ret.push(c.apply(PlCx.SCALAR, empty_args));
        }
        v_a_ref.set(v_a_val);
        v_b_ref.set(v_b_val);
        return (want == PlCx.LIST ) ? ret : ret.length_of_array()
    ';
    }
    return undef;
}

sub pairfirst (&@) {
    if (@_) {
    Java::inline '
        // PlClosure c, PlArray a
        PlObject arg = List__.shift();
        if (arg.is_lvalue()) {
            arg = arg.get();
        }
        PlClosure c = (PlClosure)arg;
        String pkg = c.pkg_name;
        PlArray ret = new PlArray();
        int size = List__.to_int();
        PlLvalue v_a_ref = (PlLvalue)PlV.sget(pkg + "::a");
        PlLvalue v_b_ref = (PlLvalue)PlV.sget(pkg + "::b");
        PlObject v_a_val = v_a_ref.get();
        PlObject v_b_val = v_b_ref.get();
        PlArray empty_args = new PlArray();
        int i = 0;
        while (i < size) {
            v_a_ref.set(List__.aget(i++));
            v_b_ref.set(List__.aget(i++));
            boolean result = c.apply(PlCx.SCALAR, empty_args).to_boolean();
            if (result) {
                ret.push(new PlArray(v_a_ref, v_b_ref));
                return (want == PlCx.LIST ) ? ret : ret.length_of_array();
            }
        }
        v_a_ref.set(v_a_val);
        v_b_ref.set(v_b_val);
        return (want == PlCx.LIST ) ? ret : ret.length_of_array()
    ';
    }
    return undef;
}

sub min     { reduce { $a < $b ? $a : $b }  @_ } 
sub max     { reduce { $a > $b ? $a : $b }  @_ } 
sub maxstr  { reduce { $a gt $b ? $a : $b } @_ } 
sub minstr  { reduce { $a lt $b ? $a : $b } @_ } 

sub sum     { reduce { $a + $b }            @_ } 
sub concat  { reduce { $a . $b }            @_ } 
sub product { reduce { $a * $b }            1, @_ } 
sub sum0    { reduce { $a + $b }            0, @_ } 

sub any     (&@) { my $code = shift; reduce { $a || $code->(local $_ = $b) }  0, @_ }
sub all     (&@) { my $code = shift; reduce { $a && $code->(local $_ = $b) }  1, @_ }
sub none    (&@) { my $code = shift; reduce { $a && !$code->(local $_ = $b) } 1, @_ }
sub notall  (&@) { my $code = shift; reduce { $a || !$code->(local $_ = $b) } 0, @_ }

sub first (&@) {
  my $code = shift;
  foreach (@_) {
    return $_ if $code->();
  }
  undef;
}

# -- To shuffle an array a of n elements (indices 0..n-1):
# for i from 0 to n−2 do
#      j ← random integer such that i ≤ j < n
#      exchange a[i] and a[j]

sub shuffle {
    my $n = @_;
    my @out = @_;
    for my $i (0 .. $n - 2) {
        my $r = int rand($n);
        ($out[$r], $out[$i]) = ($out[$i], $out[$r]);
    }
    @out;
}

sub pairkeys   { pairmap { $a }         @_ }
sub pairvalues { pairmap { $b }         @_ }
sub pairs      { pairmap { bless [ $a, $b ], 'List::Util::_Pair' } @_ }

1;

__END__


# alternate implementation

sub shuffle (@) {
  my @a=\(@_);
  my $n;
  my $i=@_;
  map {
    $n = rand($i--);
    (${$a[$n]}, $a[$n] = $a[$i])[0];
  } @_;
}

sub first (&@) {
    my $code = shift;
    reduce { defined($a)            ? $a :
             $code->(local $_ = $b) ? $b :
                                      undef
           } undef, @_
}

# quick test
#
# perl perlito5.pl -Isrc5/lib -I. -It -Cjava -e 'use Scalar::Util "refaddr"; print refaddr(\1), "\n"; use List::Util "sum", "pairmap", "pairs", "pairgrep"; say sum (4,5,6); use Data::Dumper; say Dumper [pairs (3,5,6,7)]; say Dumper [pairgrep { $a==$b } (4,5,6,7,9,9)]; '  > Main.java ; javac Main.java ; java Main
#

=head1 COPYRIGHT

The original List::Util module is

Copyright (c) 1997-2007 Graham Barr <gbarr@pobox.com>. All rights reserved.
This program is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

Recent additions and current maintenance by
Paul Evans, <leonerd@leonerd.org.uk>.

=cut

