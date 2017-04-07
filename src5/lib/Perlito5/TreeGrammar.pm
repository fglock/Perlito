package Perlito5::TreeGrammar;
use Perlito5::Dumper;
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
        Progn  => sub { Progn(@_) },     # same as in Lisp (a list of statements to execute)
        Star   => sub { Star(@_) },      # same as in regex (matches anything)
    );

# render($rule, $node)
#
# This is the entry point for walking an AST. The first argument
# is the macro rule to process, which can be thought of as a
# regular expression for trees, which means the rule itself is
# composed of smaller sub-rules.
#
# $node is the AST itself, which render() tries to match against
# $rule.
#
# Each rule is just a list, with the first element being its name,
# which in turn is one of the rulenames defined in %dispatch above.
# All elements following the rulename in a rule are interpreted as
# arguments to the rule. For instance,
# 
#     [And => [Ref => 'Perlito5::AST::Block']
#             [Lookup => 'stmts']
#             [Index => 0]
#             [Action => sub { say Perlito5::Dumper::Dumper $_[0]; }]]
#
# Will dump the first statement of a Block, if it is not empty.
#
sub render {
    my ( $rule, $node ) = @_;
    return $dispatch{ $rule->[0] }->( $rule, $node ); # note that each rule handler gets the whole rule (including its name) as first argument.
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
    # Typically, $rule->[2] contains the action to execute on a
    # successful lookup (although it could be an arbitrary rule).
    # If nothing is present, however, we just return 1 indicating
    # a successful match, else we recurse into the following rule.
    return 1 if !$rule->[2];
    return render( $rule->[2], $node->{ $rule->[1] } );
}

sub Index {
    my ( $rule, $node ) = @_;
    return
         if !ref($node)
      || ref($node) ne 'ARRAY'
      || !exists( $node->[ $rule->[1] ] );
    # See the corresponding section in Lookup()
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

