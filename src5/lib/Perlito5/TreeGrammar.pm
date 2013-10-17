package Perlito5::TreeGrammar;
use Data::Dumper;
use strict;

my %dispatch = (
        Ref    => sub { Ref(@_) },       # Ref => 'Perlito5::AST::Apply'
        Lookup => sub { Lookup(@_) },    # Lookup => 'namespace'
        Index  => sub { Index(@_) },     # Index  => '0'
        Value  => sub { Value(@_) },     # Value => '123'
        And    => sub { And(@_) },
        Or     => sub { Or(@_) },
        Not    => sub { Not(@_) },
        Action => sub { Action(@_) },
        Progn  => sub { Progn(@_) },     # same as in Lisp
        Star   => sub { Star(@_) },      # same as in regex
    );

sub render {
    my ( $rule, $node ) = @_;
    return $dispatch{ $rule->[0] }->( $rule, $node );
}

sub scan {
    my ( $rule, $node ) = @_;
    render( $rule, $node ) if $rule;
    if ( ref($node) eq 'ARRAY' ) {
        scan( $rule, $_ ) for @$node;
    }
    elsif ( ref($node) ) {
        scan( $rule, $_ ) for values %$node;
    }
    return;
}

sub Action {
    my ( $rule, $node ) = @_;
    $rule->[1]->($node);
    return 1;
}

sub Not {
    my ( $rule, $node ) = @_;
    my $result;
    render( $rule->[1], $node ) && return;
    return 1;
}

sub Star {
    my ( $rule, $node ) = @_;
    my $result;
    while (1) {
        render( $rule->[1], $node ) || return;
    }
}

sub Progn {
    my ( $rule, $node ) = @_;
    my $result;
    for ( @$rule[ 1 .. $#$rule ] ) {
        $result = render( $_, $node );
    }
    return $result;
}

sub And {
    my ( $rule, $node ) = @_;
    my $result;
    for ( @$rule[ 1 .. $#$rule ] ) {
        $result = render( $_, $node ) or return;
    }
    return $result;
}

sub Or {
    my ( $rule, $node ) = @_;
    my $result;
    for ( @$rule[ 1 .. $#$rule ] ) {
        $result = render( $_, $node ) and return $result;
    }
    return;
}

sub Ref {
    my ( $rule, $node ) = @_;
    return if ref($node) ne $rule->[1];
    return 1 if !$rule->[2];
    return render( $rule->[2], $node );
}

sub Lookup {
    my ( $rule, $node ) = @_;
    return
         if !ref($node)
      || ref($node) eq 'ARRAY'
      || !exists( $node->{ $rule->[1] } );
    return 1 if !$rule->[2];
    return render( $rule->[2], $node->{ $rule->[1] } );
}

sub Index {
    my ( $rule, $node ) = @_;
    return
         if !ref($node)
      || ref($node) ne 'ARRAY'
      || !exists( $node->[ $rule->[1] ] );
    return 1 if !$rule->[2];
    return render( $rule->[2], $node->[ $rule->[1] ] );
}

sub Value {
    my ( $rule, $node ) = @_;
    return if ref($node) || $node ne $rule->[1];
    return 1 if !$rule->[2];
    return render( $rule->[2], $node );
}

1;

