package List::Util;
use strict;

use Exporter qw(import);
our @EXPORT_OK = qw(
    reduce first
    min max minstr maxstr
    concat product sum sum0
    none notall all any
);

# TODO
#    shuffle pairmap pairgrep pairfirst pairs pairkeys pairvalues

sub reduce (&@) {
    # PerlOp.reduce(PlArray(PlClosure c, PlArray a));
    Java::inline("PerlOp.reduce(List__)");
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
    return $_ if &{$code}();
  }
  undef;
}

# sub shuffle (@) {
#   my @a=\(@_);
#   my $n;
#   my $i=@_;
#   map {
#     $n = rand($i--);
#     (${$a[$n]}, $a[$n] = $a[$i])[0];
#   } @_;
# }

1;

__END__


# alternate implementation

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

