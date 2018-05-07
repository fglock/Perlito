use v5;


package Perlito5::AST::CompUnit;
sub new {
    my $class = shift;
    my %args = @_;
    if ($args{body}) {
        my @body;
        for my $stmt ( @{ $args{body} } ) {
            next if !defined($stmt);
            next if ref($stmt) eq 'Perlito5::AST::Apply' && $stmt->{namespace} eq 'Perlito5' && $stmt->{code} eq 'nop';
            push @body, $stmt;
        }
        @body = Perlito5::Macro::split_code_too_large(@body)
            if $Perlito5::CODE_TOO_LARGE;
        $args{body} = \@body;
    }
    bless \%args, $class;
}
sub name { $_[0]->{name} }
sub body { $_[0]->{body} }



package Perlito5::AST::Block;
sub new {
    my $class = shift;
    my %args = @_;
    if ($args{stmts}) {
        my @stmts;
        for my $stmt ( @{ $args{stmts} } ) {
            next if !defined($stmt);
            next if ref($stmt) eq 'Perlito5::AST::Apply' && $stmt->{namespace} eq 'Perlito5' && $stmt->{code} eq 'nop';
            push @stmts, $stmt;
        }
        @stmts = Perlito5::Macro::split_code_too_large(@stmts)
            if $Perlito5::CODE_TOO_LARGE;
        $args{stmts} = \@stmts;
    }
    bless \%args, $class;
}
sub sig { $_[0]->{sig} }
sub stmts { $_[0]->{stmts} }



package Perlito5::AST::Int;
sub new { my $class = shift; bless {@_}, $class }
sub int { $_[0]->{int} }
sub value { $_[0]->{int} }



package Perlito5::AST::Num;
sub new { my $class = shift; bless {@_}, $class }
sub num { $_[0]->{num} }
sub value { $_[0]->{num} }



package Perlito5::AST::Buf;
sub new { my $class = shift; bless {@_}, $class }
sub buf { $_[0]->{buf} }
sub value { $_[0]->{buf} }



package Perlito5::AST::Index;
sub new { my $class = shift; bless {@_}, $class }
sub obj { $_[0]->{obj} }
sub index_exp { $_[0]->{index_exp} }

sub INDEX {
    my ($term, $index) = @_;
    if (ref($term) eq 'Perlito5::AST::Var' && $term->{sigil} eq '@') {
        return Perlito5::AST::Index->new(
            obj => Perlito5::AST::Var->new( %$term, _real_sigil => $term->{sigil}, sigil => '$' ),
            index_exp => $index,
        );
    }
    return Perlito5::AST::Call->new(
        method => 'postcircumfix:<[ ]>',
        invocant => $term,
        arguments => $index,
    );
}


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

    if ((ref($index) eq 'Perlito5::AST::Apply')
       && $index->{bareword}
       )
    {
        my $full_name = ($index->{namespace} ? $index->{namespace} . '::' : "") . $index->{code};
        if ( !exists $Perlito5::PROTO->{$full_name} ) {
            return Perlito5::AST::Buf->new( buf => $full_name );
        }
    }
    elsif (  (ref($index) eq 'Perlito5::AST::Apply')
          && ($index->code eq 'prefix:<->' || $index->code eq 'prefix:<+>')
          )
    {
        my $arg = $index->arguments->[0];
        return Perlito5::AST::Apply->new(
                    code => $index->code,
                    namespace => $index->namespace, 
                    arguments => [ $self->autoquote($arg) ],
               )
            if $arg;
    }
    elsif (  (ref($index) eq 'Perlito5::AST::Apply')
          && ($index->code eq 'list:<,>')
          )
    {

        my $obj = $self->obj;
        if ($obj->sigil eq '@') {
            #  @v{ $a, $b, $c }
            return $index;
        }

        #  $v{ $a, $b, $c }
        my $args = $index->arguments;
        return Perlito5::AST::Apply->new(
                    code => 'join',
                    namespace => '', 
                    arguments => [
                        Perlito5::AST::Var->new( name => ';', namespace => '', sigil => '$' ),
                        map { defined($_) ? $_
                                          : Perlito5::AST::Buf->new( buf => '' )
                            }
                            @$args
                    ],
               );
    }

    $index;
}

sub LOOKUP {
    my ($term, $index) = @_;
    if (ref($term) eq 'Perlito5::AST::Var' && $term->{sigil} eq '%') {
        return Perlito5::AST::Lookup->new(
            obj => Perlito5::AST::Var->new( %$term, _real_sigil => $term->{sigil}, sigil => '$' ),
            index_exp => $index,
        );
    }
    return Perlito5::AST::Call->new(
        method => 'postcircumfix:<{ }>',
        invocant => $term,
        arguments => $index,
    );
}

package Perlito5::AST::Var;
sub new {
    my ($class, %args) = @_;
    my $var = bless \%args, $class;
    push @Perlito5::SCOPE_STMT, $var;
    return $var;
}
sub sigil { $_[0]->{sigil} }
sub namespace { $_[0]->{namespace} }
sub name { $_[0]->{name} }

sub clone {
    my $self = shift;
    return bless { %$self }, ref($self);
}

sub plain_name {
    my $self = shift;
    if ($self->namespace) {
        return $self->namespace . '::' . $self->name
    }
    return $self->name
}

our %Special_var = (
    ARGV => 1,
    INC  => 1,
    ENV  => 1,
    SIG  => 1,
    _    => 1,
);
our %NonSpecial_var = map { $_ => 1 } (
    'A'..'Z', '_', 'a'..'z'
);
sub is_special_var {
    #  $1 %ENV $_ $/
    my $self = shift;
    my $c = substr($self->{name}, 0, 1);
    if ( $Special_var{ $self->{name} } || !$NonSpecial_var{$c} ) {
        return 1;
    }
    0;
}

sub SCALAR_ARG {
    Perlito5::AST::Var->new(
        sigil     => '$',
        namespace => '',
        name      => '_',
        '_decl'   => 'global',
        '_namespace' => 'main',
    );
}

sub LIST_ARG {
    Perlito5::AST::Var->new(
        sigil     => '@',
        namespace => '',
        name      => '_',
        '_decl'   => 'global',
        '_namespace' => 'main',
    );
}

sub LIST_ARG_INDEX {
    my $index = shift;
    Perlito5::AST::Index::INDEX( LIST_ARG(), Perlito5::AST::Int->new( int => $index ) );
}

package Perlito5::AST::Call;
sub new { my $class = shift; bless {@_}, $class }
sub invocant { $_[0]->{invocant} }
sub method { $_[0]->{method} }
sub arguments { $_[0]->{arguments} }



package Perlito5::AST::Apply;
sub new { my $class = shift; bless {@_}, $class }
sub code        { $_[0]->{code}        }    # print
sub special_arg { $_[0]->{special_arg} }    # STDOUT
sub arguments   { $_[0]->{arguments}   }    # 1,2,3
sub namespace   { $_[0]->{namespace}   }    # CORE
# ignore_proto                              # &mysub

sub PUSH {
    my ($var, $value) = @_;
    if (ref($var) eq 'Perlito5::AST::Var' && $var->{sigil} eq '@') {
        return Perlito5::AST::Apply->new(
            code => 'push',
            arguments => [$var, $value],
        );
    }
    return Perlito5::AST::Apply->new(
        code => 'push',
        arguments => [
            Perlito5::AST::Apply->new(
                code => 'prefix:<@>',
                arguments => [$var],
            ),
            $value,
        ],
    );
}


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
sub topic { $_[0]->{topic} }



package Perlito5::AST::Given;
sub new { my $class = shift; bless {@_}, $class }
sub cond { $_[0]->{cond} }
sub body { $_[0]->{body} }



package Perlito5::AST::Decl;
sub new { my $class = shift; bless {@_}, $class }
sub decl { $_[0]->{decl} }
sub type { $_[0]->{type} }
sub var { $_[0]->{var} }
sub attributes { $_[0]->{attributes} }



package Perlito5::AST::Sub;
sub new { my $class = shift; bless {@_}, $class }
sub name { $_[0]->{name} }
sub sig { $_[0]->{sig} }
sub block { $_[0]->{block} }
sub attributes { $_[0]->{attributes} }

sub is_named_sub {
    my $self = shift;
    (ref($self) eq 'Perlito5::AST::Sub') && $self->{name}
}
sub is_anon_sub {
    my $self = shift;
    (ref($self) eq 'Perlito5::AST::Sub') && !$self->{name}
}


1;

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
