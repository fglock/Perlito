use v5;


package Perlito5::AST::CompUnit;
sub new { my $class = shift; bless {@_}, $class }
sub name { $_[0]->{name} }
sub body { $_[0]->{body} }



package Perlito5::AST::Val::Int;
sub new { my $class = shift; bless {@_}, $class }
sub int { $_[0]->{int} }



package Perlito5::AST::Val::Num;
sub new { my $class = shift; bless {@_}, $class }
sub num { $_[0]->{num} }



package Perlito5::AST::Val::Buf;
sub new { my $class = shift; bless {@_}, $class }
sub buf { $_[0]->{buf} }



package Perlito5::AST::Lit::Block;
sub new { my $class = shift; bless {@_}, $class }
sub sig { $_[0]->{sig} }
sub stmts { $_[0]->{stmts} }



package Perlito5::AST::Index;
sub new { my $class = shift; bless {@_}, $class }
sub obj { $_[0]->{obj} }
sub index_exp { $_[0]->{index_exp} }



package Perlito5::AST::Lookup;
sub new { my $class = shift; bless {@_}, $class }
sub obj { $_[0]->{obj} }
sub index_exp { $_[0]->{index_exp} }

sub autoquote {
    my $self  = shift;
    my $index = shift;

    # ok   ' sub x () { 123 } $v{x()} = 12; use Data::Dumper; print Dumper \%v '       # '123'     => 12
    # ok   ' sub x () { 123 } $v{x} = 12; use Data::Dumper; print Dumper \%v '         # 'x'       => 12
    # TODO ' sub x () { 123 } $v{main::x} = 12; use Data::Dumper; print Dumper \%v '   # '123'     => 12
    # ok   ' $v{main::x} = 12; use Data::Dumper; print Dumper \%v '                    # 'main::x' => 12

    if ($index->isa('Perlito5::AST::Apply')
       && $index->{bareword}
       )
    {
        return Perlito5::AST::Val::Buf->new( buf => ($index->{namespace} ? $index->{namespace} . '::' : "") . $index->{code} );
    }

    $index;
}



package Perlito5::AST::Var;
sub new { my $class = shift; bless {@_}, $class }
sub sigil { $_[0]->{sigil} }
sub namespace { $_[0]->{namespace} }
sub name { $_[0]->{name} }

sub plain_name {
    my $self = shift;
    if ($self->namespace) {
        return $self->namespace . '::' . $self->name
    }
    return $self->name
}




package Perlito5::AST::Proto;
sub new { my $class = shift; bless {@_}, $class }
sub name { $_[0]->{name} }



package Perlito5::AST::Call;
sub new { my $class = shift; bless {@_}, $class }
sub invocant { $_[0]->{invocant} }
sub method { $_[0]->{method} }
sub arguments { $_[0]->{arguments} }



package Perlito5::AST::Apply;
sub new { my $class = shift; bless {@_}, $class }
sub code { $_[0]->{code} }
sub arguments { $_[0]->{arguments} }
sub namespace { $_[0]->{namespace} }



package Perlito5::AST::If;
sub new { my $class = shift; bless {@_}, $class }
sub cond { $_[0]->{cond} }
sub body { $_[0]->{body} }
sub otherwise { $_[0]->{otherwise} }



package Perlito5::AST::When;
sub new { my $class = shift; bless {@_}, $class }
sub cond { $_[0]->{cond} }
sub body { $_[0]->{body} }



package Perlito5::AST::While;
sub new { my $class = shift; bless {@_}, $class }
sub init { $_[0]->{init} }
sub cond { $_[0]->{cond} }
sub continue { $_[0]->{continue} }
sub body { $_[0]->{body} }



package Perlito5::AST::For;
sub new { my $class = shift; bless {@_}, $class }
sub cond { $_[0]->{cond} }
sub continue { $_[0]->{continue} }
sub body { $_[0]->{body} }



package Perlito5::AST::Decl;
sub new { my $class = shift; bless {@_}, $class }
sub decl { $_[0]->{decl} }
sub type { $_[0]->{type} }
sub var { $_[0]->{var} }



package Perlito5::AST::Sig;
sub new { my $class = shift; bless {@_}, $class }
sub positional { $_[0]->{positional} }



package Perlito5::AST::Sub;
sub new { my $class = shift; bless {@_}, $class }
sub name { $_[0]->{name} }
sub sig { $_[0]->{sig} }
sub block { $_[0]->{block} }



package Perlito5::AST::Do;
sub new { my $class = shift; bless {@_}, $class }
sub block { $_[0]->{block} }



package Perlito5::AST::Use;
sub new { my $class = shift; bless {@_}, $class }
sub mod { $_[0]->{mod} }
sub code { $_[0]->{code} }



=begin

=head1 NAME

Perlito5::AST - Base class for Perlito Perl5 AST nodes

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
