
use v6;

module Perlito5::Perl6::Runtime {


# bless() contributed by not_gerd in #perl6
#
# role Foo { method bar { say self<spam>; } }; 
# my $x = { spam => 'eggs' };
# bless5($x, 'Foo'); $x.bar;
sub bless ($obj, $role) {
    $obj does ::($role)
}

# class X { has $.val }; my $v = {}; bless($v, "X"); $v.WHAT.say
# sub bless ($v is rw, $class) {
#     $v = ::($class).new(val=>$v)
# }

sub ref ($obj) { #OK
    my $s = $obj.WHAT.perl;
    return 'ARRAY' if $s eq 'Array';
    return 'HASH'  if $s eq 'Hash';
    return ''      if $s eq 'Str';
    return ''      if $s eq 'Int';
    return ''      if $s eq 'Rat';
    # TODO - REF, CODE, ...
    return $s;
}

}

=begin

=head1 NAME

Perlito5::Perl5::Runtime

=head1 DESCRIPTION

Provides runtime routines for the Perlito5-in-Perl6 compiled code

=head1 AUTHORS

The Pugs Team.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011, 2012 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
