use v5;


package CompUnit;
sub new { my $class = shift; bless {@_}, $class }
sub name { $_[0]->{'name'} }
sub body { $_[0]->{'body'} }



package Val::Int;
sub new { my $class = shift; bless {@_}, $class }
sub int { $_[0]->{'int'} }



package Val::Num;
sub new { my $class = shift; bless {@_}, $class }
sub num { $_[0]->{'num'} }



package Val::Buf;
sub new { my $class = shift; bless {@_}, $class }
sub buf { $_[0]->{'buf'} }



package Lit::Block;
sub new { my $class = shift; bless {@_}, $class }
sub sig { $_[0]->{'sig'} }
sub stmts { $_[0]->{'stmts'} }



package Index;
sub new { my $class = shift; bless {@_}, $class }
sub obj { $_[0]->{'obj'} }
sub index_exp { $_[0]->{'index_exp'} }



package Lookup;
sub new { my $class = shift; bless {@_}, $class }
sub obj { $_[0]->{'obj'} }
sub index_exp { $_[0]->{'index_exp'} }



package Var;
sub new { my $class = shift; bless {@_}, $class }
sub sigil { $_[0]->{'sigil'} }
sub namespace { $_[0]->{'namespace'} }
sub name { $_[0]->{'name'} }



package Proto;
sub new { my $class = shift; bless {@_}, $class }
sub name { $_[0]->{'name'} }



package Call;
sub new { my $class = shift; bless {@_}, $class }
sub invocant { $_[0]->{'invocant'} }
sub method { $_[0]->{'method'} }
sub arguments { $_[0]->{'arguments'} }



package Apply;
sub new { my $class = shift; bless {@_}, $class }
sub code { $_[0]->{'code'} }
sub arguments { $_[0]->{'arguments'} }
sub namespace { $_[0]->{'namespace'} }



package If;
sub new { my $class = shift; bless {@_}, $class }
sub cond { $_[0]->{'cond'} }
sub body { $_[0]->{'body'} }
sub otherwise { $_[0]->{'otherwise'} }



package While;
sub new { my $class = shift; bless {@_}, $class }
sub init { $_[0]->{'init'} }
sub cond { $_[0]->{'cond'} }
sub continue { $_[0]->{'continue'} }
sub body { $_[0]->{'body'} }



package For;
sub new { my $class = shift; bless {@_}, $class }
sub cond { $_[0]->{'cond'} }
sub body { $_[0]->{'body'} }



package Decl;
sub new { my $class = shift; bless {@_}, $class }
sub decl { $_[0]->{'decl'} }
sub type { $_[0]->{'type'} }
sub var { $_[0]->{'var'} }



package Sig;
sub new { my $class = shift; bless {@_}, $class }
sub invocant { $_[0]->{'invocant'} }
sub positional { $_[0]->{'positional'} }
sub named { $_[0]->{'named'} }



package Sub;
sub new { my $class = shift; bless {@_}, $class }
sub name { $_[0]->{'name'} }
sub sig { $_[0]->{'sig'} }
sub block { $_[0]->{'block'} }



package Do;
sub new { my $class = shift; bless {@_}, $class }
sub block { $_[0]->{'block'} }



package Use;
sub new { my $class = shift; bless {@_}, $class }
sub mod { $_[0]->{'mod'} }


=begin

=head1 NAME

Perlito5::AST - Base class for Perlito AST nodes

=head1 DESCRIPTION

This module provides AST node class declarations for the Perlito Perl 5 compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.

=head1 COPYRIGHT

Copyright 2011, 2012 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
