package List::Util;
use strict;

use Exporter qw(import);
our @EXPORT_OK = qw(
    reduce first
    min max minstr maxstr
    concat product sum sum0
    none notall all any
    shuffle
);

# TODO - pairmap pairgrep pairfirst pairs pairkeys pairvalues

sub reduce (&@) {
    if (@_) {
    Java::inline '
        // PlClosure c, PlArray a
        PlObject arg = List__.shift();
        PlClosure c = (PlClosure)arg;
        // List::Util reduce()
        // TODO - pass @_ to the closure
        // TODO - use "\@" signature for better performance
        String pkg = c.pkg_name;
        PlObject ret = PlCx.UNDEF;
        int size = List__.to_int();
        if (size == 0) {
            return PlCx.UNDEF;
        }
        if (size == 1) {
            return List__.aget(0);
        }
        PlLvalue v_a_ref = (PlLvalue)PlV.get(pkg + "::v_a");
        PlLvalue v_b_ref = (PlLvalue)PlV.get(pkg + "::v_b");
        PlObject v_a_val = v_a_ref.get();
        PlObject v_b_val = v_b_ref.get();
        v_a_ref.set(List__.aget(0));
        for (int i = 1; i < size; i++) {
            v_b_ref.set(List__.aget(i));
            v_a_ref.set(c.apply(PlCx.SCALAR, new PlArray()));
        }
        ret = v_a_ref.get();
        v_a_ref.set(v_a_val);
        v_b_ref.set(v_b_val);
        return ret
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



=head1 COPYRIGHT

The original List::Util module is

Copyright (c) 1997-2007 Graham Barr <gbarr@pobox.com>. All rights reserved.
This program is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

Recent additions and current maintenance by
Paul Evans, <leonerd@leonerd.org.uk>.

=cut

