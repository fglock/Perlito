use v5;
package Perlito5::Macro;
use strict;
use Perlito5::Clone;

# provide "goto LABEL" within a statement list
#
#  {
#       123;
#       my $var;
#       goto LABEL;
#       123;
#     LABEL:
#       456;
#  }
#
#  a "forward goto" becomes:

#  LABEL:
#  {
#       123;
#       my $var;
#       do { 456; last LABEL; };
#       123;
#       456;
#  }

# See:
#   https://jamey.thesharps.us/2016/05/09/how-to-eliminate-goto-statements/

sub rewrite_goto {
    my ($stmts) = @_;
    return $stmts if !@Perlito5::GOTO;  # no "goto"

    # ignore "goto &sub" - this is processed elsewhere
    # ignore "goto $str" - this is not yet implemented - TODO
    # accept "goto LABEL" (bareword)
    @Perlito5::GOTO = grep {
        $_->{arguments}[0]{bareword}
    } @Perlito5::GOTO;
    return $stmts if !@Perlito5::GOTO;

    my $change = 0;
    my $outer_label = Perlito5::get_label();
    my @unprocessed;
  GOTO:
    for my $goto (@Perlito5::GOTO) {
        my $label = $goto->{arguments}[0]{code};
        next GOTO unless $label;

        # lookup for labels
        my @label;
        for my $id (reverse (0 .. $#$stmts)) {
            my $ast = $stmts->[$id];
            if ($ast->{label} && $ast->{label} eq $label) {
                my @stmt_list = @{$stmts}[ $id .. $#$stmts ];
                # warn "Block uses goto: ", Perlito5::Dumper::Dumper($goto);
                # warn "Block goes here: ", Perlito5::Dumper::Dumper(\@stmt_list);

                if ( $id == 0 ) {
                    # if $id == 0, replace "goto" with "redo"
                    $goto->{code} = "redo";
                    $goto->{arguments} =[
                        Perlito5::AST::Apply->new(
                            code => $outer_label,
                            bareword => 1,
                        )
                    ];
                    $change = 1;
                    next GOTO;
                }

                # TODO - this doesn't cover all cases

                # do { @stmt; last LABEL };
                $goto->{code} = "do";
                $goto->{arguments} = [
                    Perlito5::AST::Block->new(
                        stmts => [
                            @{ Perlito5::Clone::clone(\@stmt_list) },
                            Perlito5::AST::Apply->new(
                                code => "last",
                                arguments => [
                                    Perlito5::AST::Apply->new(
                                        code => $outer_label,
                                        bareword => 1,
                                    ),
                                ],
                            ),
                        ],
                    ),
                ];
                $change = 1;
                next GOTO;
            }
        }
        push @unprocessed, $goto;
    }
    @Perlito::GOTO = @unprocessed;

    if ($change) {
        # wrap the statements in a block with the new label
        return [
            Perlito5::AST::Block->new(
                label => $outer_label,
                stmts => [ @$stmts ],
            ),
        ];
    }

    return $stmts;
}


{
package Perlito5::AST::Apply;
use strict;

my %op = (
    'infix:<+=>'  => 'infix:<+>',
    'infix:<-=>'  => 'infix:<->',
    'infix:<*=>'  => 'infix:<*>',
    'infix:</=>'  => 'infix:</>',
    'infix:<%=>'  => 'infix:<%>',
    'infix:<||=>' => 'infix:<||>',
    'infix:<&&=>' => 'infix:<&&>',
    'infix:<|=>'  => 'infix:<|>',
    'infix:<&=>'  => 'infix:<&>',
    'infix:<^=>'  => 'infix:<^>',
    'infix:<//=>' => 'infix:<//>',
    'infix:<**=>' => 'infix:<**>',
    'infix:<.=>'  => 'list:<.>',
    'infix:<x=>'  => 'infix:<x>',
    'infix:<<<=>' => 'infix:<<<>',
    'infix:<>>=>' => 'infix:<>>>',
    'infix:<&.=>' => 'infix:<&.>',
    'infix:<|.=>' => 'infix:<|.>',
    'infix:<^.=>' => 'infix:<^.>',
);

sub op_assign {
    my $self = $_[0];

    my $code = $self->{code};
    return 0 if ref($code);

    if (exists( $op{$code} )) {
        return Perlito5::AST::Apply->new(
            code      => 'infix:<=>',
            arguments => [
                $self->{arguments}->[0],
                Perlito5::AST::Apply->new(
                    code      => $op{$code},
                    arguments => $self->{arguments},
                    ( $self->{_integer} ? ( _integer => 1 ) : () ),
                ),
            ]
        );
    }

    return 0;
}

my %op_auto = (
    'prefix:<++>'  => 1,
    'prefix:<-->'  => 1,
    'postfix:<++>' => 1,
    'postfix:<-->' => 1,
);

sub op_auto {
    my $self = $_[0];

    my $code = $self->{code};
    return 0 if ref($code);

    if (exists( $op_auto{$code} )) {
        #   ++( $v = 2 )
        #   do { $v = 2; ++$v }

        my $paren = $self->{arguments}[0];
        if ($paren->{code} eq 'circumfix:<( )>') {

            my $arg = $paren->{arguments}[-1];
            if ($arg->{code} eq 'infix:<=>') {

                my $var = $arg->{arguments}[0];

                return Perlito5::AST::Apply->new(
                    code => 'do',
                    arguments => [ Perlito5::AST::Block->new(
                        stmts => [
                            $paren,     # assignment
                            Perlito5::AST::Apply->new(
                                code => $code,  # autoincrement
                                arguments => [ $var ],
                            ),
                        ],
                    ) ],
                );
            }
        }
    }

    return 0;
}

} # /package


{
package Perlito5::AST::Sub;
use strict;
use feature 'say';
use Perlito5::TreeGrammar;
# Transformation for implementing state variables (perldoc -f state) by
# enclosing the sub in a do block, which in turn contains lexical variables
# that corresponding to the state variables in use in the sub. Returns a `do`
# expr node enclosing the modified sub, or 0 if no statevars are found in $self.
sub maybe_rewrite_statevars {
    my ($self) = @_;
    my $block = $self->{block} || return 0;
    my @init_flags; # Holds the *names* of intialization flags for state vars.
    my @vars;
    my @base_rules = ([Lookup => 'block'], [Lookup => 'stmts']); # $self->{block}{stmts}
    for my $idx (0..$#{ $block->{stmts} }) {
        my $stmt = $block->{stmts}[$idx];
        my ($node, @rules) = find_state_expr($stmt, @base_rules, [Index => $idx]);
        if (defined $node) {
            my ($transformed, $var, $flagvar) = rewrite_state_expr($node);
            # Everything in @vars will be added to a big `my` decl block. See below.
            push @vars, $var, $flagvar;
            # Now we will replace $node in the tree with $transformed.
            # To do that, we note that the last rule in @rules *must*
            # be an Index or a Lookup. We pop this last rule into
            # $last_rule, and then push an Action rule into
            # @rules. This Action rule will be fired when the rules
            # before it in @rules have all executed, resulting in the
            # parent of $node being passed to the Action sub. At that
            # point, we take this parent node (say, $parent), and
            # depending on whether $last_rule->[0] is a Lookup or an Index,
            # we do $parent->{$last_rule->[1]} = $transformed or
            # $parent->[$last_rule->[1]] = $transformed.
            my $last_rule = pop @rules;
            push @rules, [Action => sub {
                my ($parent) = @_; # This node will be the parent of $node.
                if ($last_rule->[0] eq 'Lookup') {
                    $parent->{$last_rule->[1]} = $transformed;
                } else {
                    $parent->[$last_rule->[1]] = $transformed;
                }
            }];
            # This is the nested rule built from @rules, that correctly
            # walks the AST till the parent of $node.
            my $rule = nest(\@rules);
            Perlito5::TreeGrammar::render($rule, $self);
        }
    }
    # If there are any state var expressions that we have rewritten,
    # we shall return a `do` block containing a `my` declaraction of
    # the transformed variables followed by the mutated Sub node for
    # $self.
    if (scalar @vars) {
        return Perlito5::AST::Apply->new(
            code      => 'do',
            namespace => $block->{namespace},
            arguments => [Perlito5::AST::Block->new(
                sig   => undef,
                stmts => [
                    myvar_declaration_stmt(@vars),
                    $self,
                ]
            )],
        );
    }
    return 0;
}

# myvar_declaration_stmt($var_node1, $var_node2, ...)
#
# Returns a `my` declaration node from a bunch of Var nodes, equivalent to the
# statement
#   
#   my ($v1, $v2, ...);
#
# where $v1, $v2 etc are the names of the passed in Var nodes.
#
sub myvar_declaration_stmt {
    die 'Can only handle `my` variables'
        if (grep { ($_->{decl} // 'my') ne 'my' } @_);
    return map {
        Perlito5::AST::Decl->new(
            decl  => 'my',
            var   => $_,
        )
    } @_;
}

# rewrite_state_expr($refnode)
#
# Given an AST node that is a state variable declaration/assignment,
# returns a list of three items:
# 
#    - $transformed  The transformed node. Specifically, a state declaration or assignment is
#                    transformed to a `do` block that closes over a `my` variable with the same
#                    name as the original state variable. An additional flag variable is created
#                    for initializing this closed over `my` variable exactly once.
#   
#    - $var          An *AST node* representing the aforementioned `my` variable. It is the job
#                    of the caller to place this in a scope that can be closed over by
#                    $transformed.
#
#    - $flagvar      The flag variable node, which must also be placed in the enclosing scope
#                    of $transformed.
#
sub rewrite_state_expr {
    my ($target) = @_;
    my ($decl, $rhs);
    if (ref $target eq 'Perlito5::AST::Apply') {
        $decl = $target->{arguments}[0];
        $rhs = $target->{arguments}[1];
    } elsif (ref $target eq 'Perlito5::AST::Decl') {
        $decl = $target;
    } else {
        die "Invalid node type for state variable transformation: " . (ref $target);
    }
    my $state_var = $decl->{var};
    # This is the lexical variable that we'll inject into the surrounding
    # do block
    my $var = Perlito5::AST::Var->new(
        namespace => $state_var->{namespace},
        sigil     => $state_var->{sigil},
        name      => $state_var->{name},
        _id       => $state_var->{_id},
        _decl     => 'my',
    );
    # A label to ensure hygienic names for the initialization flag.
    my $label = Perlito5::get_label();
    # The flag variable which will go in to the surrounding `do` block.
    my $flagvar = Perlito5::AST::Var->new(
        name      => $var->{name} . "_inited_" . $label, # e.g., x_initied_tmp404
        sigil     => '$',
        namespace => '',
        _id       => $Perlito5::ID++,
        _decl     => 'my',
    );
    # The `do` block that actually checks for the init flag and initializes
    # the "state" variable (and of course sets the flag).
    my $init_block = Perlito5::AST::Apply->new(
        code      => 'do',
        namespace => $decl->{namespace},
        arguments => [Perlito5::AST::Block->new(
            sig   => undef,
            stmts => [
                # First statement in the do block sets the init flag to 1.
                Perlito5::AST::Apply->new(
                    code      => 'infix:<=>',
                    namespace => $decl->{namespace},
                    arguments => [
                        $flagvar,
                        Perlito5::AST::Int->new(int => 1),
                    ],
                ),
                # Next, we set the variable to whatever the original state
                # variable was set to. This being the last statement of the
                # do block, it will also become the value of the block.
                # If $target points to an Apply node, then we also construct
                # an apply node, else we leave it as a my decl.
                ((ref $target eq 'Perlito5::AST::Apply') ?
                    Perlito5::AST::Apply->new(
                        code      => 'infix:<=>',
                        namespace => $decl->{namespace},
                        arguments => [$var, $rhs],
                    ) : $var),
            ],
        )],
    );
    # Replace the Apply node with a ternary op that checks if the state
    # variable has been initialized and initializes it using the above `do`
    # block if not.
    my $transformed = Perlito5::AST::Apply->new(
        code      => 'ternary:<? :>',
        namespace => $decl->{namespace},
        arguments => [$flagvar, $var, $init_block],
    );
    # Return our new code and all the variables that need to be in scope for
    # the block to work.
    return ($transformed, $var, $flagvar);
}

# nest(ARRAYREF)
#
# Given an arrayref of the form
#
#    [ [a1, a2, a3, ...], [b1, b2, b3, ...], [c1, c2, c3, ...] ]
#
# produces an arrayref of nested arrayrefs, with the first element of each
# flattened to one level:
# 
#    [ a1, a2, a3, ..., [b1, b2, b3, ..., [c1, c2, c3, ... [...]]]]
# 
# As a concrete example,
#    
#    [[ Lookup => 'blah' ], [ Index => 0 ], [ Lookup => 'bar' ], [ Action => $sub ]]
# 
# becomes
# 
#    [ Lookup => 'blah', [ Index => 0, [ Lookup => 'bar', [ Action => $sub ]]]]
#
# Note the flattening is only upto one level, e.g., 
#   
#    [[a, [b, c]], [d, e]]
#
# becomes
#
#    [a, [b, c], [d, e]]
# 
# and not
#
#    [a, b, c, [d, e]]
#
sub nest {
    my ($xs) = @_;
    if (scalar @$xs == 0) {
        return;
    }
    return [flatten($xs->[0]), nest([@{$xs}[1..$#$xs]])];
}

# flatten(ARRAYREF|SCALAR)
#
# If $x is an ARRAYREF, return @$x, otherwise returns $x.
sub flatten {
    my ($arg) = @_;
    if (ref $arg eq 'ARRAY') {
        return (@{$arg});
    }
    return $arg;
}

# find_state_expr($node, @rules)
#
# Find a state variable declaration in the AST subtree rooted at
# $node.  @rules is a *list* of Perlito5::TreeGrammar rules (and not a
# single monolithic rule tree) required to reach $node.
#
# If a state variable declaration/initialization expression is found,
# this function returns the state decl node and the rule list required
# get to it.
#
# If @rules is empty, the returned list of rules will be the rules
# required to get to the found state variable decl/init node *from*
# $node.
#
# If no state variable declaration/initialization is found, undef is
# returned.
sub find_state_expr {
    my ($node, @rules) = @_;
    if (is_node_state_decl($node) || is_node_state_assignment($node)) {
        return ($node, @rules);
    }
    # Recursively scan each node in $node->{stmts} or $node->{arguments},
    # if we can.
    for my $branch (qw(stmts arguments)) {
        if (exists $node->{$branch} && ref $node->{$branch} eq 'ARRAY'){
            for (0..$#{$node->{$branch}}) {
                my ($retnode, @retrules) = find_state_expr(
                    $node->{$branch}[$_],
                    # The rules are the rules to reach $node
                    # plus the rules to reach $node->{$branch}{$_}
                    # from $node.
                    @rules, [Lookup => $branch], [Index => $_]
                );
                if ($retnode) {
                    return ($retnode, @retrules);
                }
            }
        }
    }
    return undef;
}

# Check if $node represents code of the form `state $var = <something>`
sub is_node_state_assignment {
    my ($node) = @_;
    (ref $node eq 'Perlito5::AST::Apply' &&
     $node->{code} eq 'infix:<=>' &&
     is_node_state_decl($node->{arguments}[0]));
}
# Is the given node a state variable declaration.
sub is_node_state_decl {
    my ($node) = @_;
    (ref $node eq 'Perlito5::AST::Decl' && $node->{decl} eq 'state');
}
} # package Perlito5::AST::Sub


sub while_file {
    my $self = $_[0];
    return 0
        if ref($self) ne 'Perlito5::AST::While';
    my $cond = $self->{cond};
    if ($cond->isa('Perlito5::AST::Apply') && ($cond->{code} eq 'readline')) {
        # while (<>) ...  is rewritten as  while ( defined($_ = <>) ) { ...
        $self->{cond} = bless({
                'arguments' => [
                    bless({
                        'arguments' => [
                            Perlito5::AST::Var::SCALAR_ARG(),
                            $cond,
                        ],
                        'code' => 'infix:<=>',
                        'namespace' => '',
                    }, 'Perlito5::AST::Apply'),
                ],
                'bareword' => '',
                'code' => 'defined',
                'namespace' => '',
            }, 'Perlito5::AST::Apply');
        return $self;
    }
    return 0;
}

sub insert_return_in_block {
    my ($self) = @_;
    if (@{$self->{stmts}} == 0) {
        push @{$self->{stmts}},
                        Perlito5::AST::Apply->new(
                            'arguments' => [],
                            'code' => 'return',
                            'namespace' => '',
                            '_return_from_block' => 1,
                        );
    }
    else {
        my $last_statement = pop(@{$self->{stmts}});
        push(@{$self->{stmts}}, insert_return( $last_statement ));
    }
    return $self;
}
sub insert_return_in_if {
    my $self = $_[0];
    $self->{body}      = insert_return_in_block($self->{body} || Perlito5::AST::Block->new(stmts => []));
    $self->{otherwise} = insert_return_in_block($self->{otherwise} || Perlito5::AST::Block->new(stmts => []));
    return $self;
}
sub insert_return {
    my $self = $_[0];
    if ($self->isa('Perlito5::AST::If')) {
        return insert_return_in_if($self);
    }
    if ($self->isa('Perlito5::AST::Block')) {
        return insert_return_in_block($self);
    }
    if ($self->isa('Perlito5::AST::For')) {
        return (
            $self,
            Perlito5::AST::Apply->new(
                'arguments' => [ Perlito5::AST::Buf->new( buf => "" ) ],
                'code' => 'return',
                'namespace' => '',
                '_return_from_block' => 1,
            ),
        );
    }
    if ( $self->isa('Perlito5::AST::While') ) {
        if (   $self->{cond}->isa('Perlito5::AST::Int')  && $self->{cond}{int} )
        {
            # do not emit "return" after while(1){...} because "unreachable statement"
            return $self;
        }
        else {
            return (
                $self,
                Perlito5::AST::Apply->new(
                    'arguments' => [ Perlito5::AST::Int->new( int => 0 ) ],
                    'code' => 'return',
                    'namespace' => '',
                    '_return_from_block' => 1,
                ),
            );
        }
    }
    if ( $self->isa( 'Perlito5::AST::Sub' ) ) {
        if ( ! $self->{name} )
        {
            return Perlito5::AST::Apply->new(
                    'arguments' => [ $self ],
                    'code' => 'return',
                    'namespace' => '',
                    '_return_from_block' => 1,
                );
        }
        else {
            return (
                $self,
                Perlito5::AST::Apply->new(
                    'arguments' => [ Perlito5::AST::Int->new( int => 0 ) ],
                    'code' => 'return',
                    'namespace' => '',
                    '_return_from_block' => 1,
                ),
            );
        }
    }
    if (   $self->isa( 'Perlito5::AST::Int' )
        || $self->isa( 'Perlito5::AST::Num' )
        || $self->isa( 'Perlito5::AST::Buf' )
        || $self->isa( 'Perlito5::AST::Index' )
        || $self->isa( 'Perlito5::AST::Lookup' )
        || $self->isa( 'Perlito5::AST::Call' )
        || $self->isa( 'Perlito5::AST::Var' )
        || $self->isa( 'Perlito5::AST::Decl' )
    ) {
        return Perlito5::AST::Apply->new(
                'arguments' => [ $self ],
                'code' => 'return',
                'namespace' => '',
                '_return_from_block' => 1,
            );
    }
    if ( $self->isa( 'Perlito5::AST::Apply' ) ) {
        if ( $self->code eq 'return' ) {
            return $self;
        }
        return Perlito5::AST::Apply->new(
                'arguments' => [ $self ],
                'code' => 'return',
                'namespace' => '',
                '_return_from_block' => 1,
            );
    }
    return $self;
}


# sub split_deep_if {
#     my $stmt = $_[0];
# 
#     if (ref($stmt) eq 'Perlito5::AST::If' && $stmt->{otherwise} && $stmt->{otherwise}->isa('Perlito5::AST::Block')) {
#         # if ... else if ...
#         my $stmts = $stmt->{otherwise}{stmts};
#         my $v = $stmts;
#         if ($stmts && @$stmts == 1) {
#             my $stmt = $stmts->[0];
#             if (ref($stmt) eq 'Perlito5::AST::If' && $stmt->{otherwise} && $stmt->{otherwise}->isa('Perlito5::AST::Block')) {
#                 # if ... else if ...
#                 ### my $stmts = $stmt->{otherwise}{stmts};
#                 ### if ($stmts && @$stmts == 1) {
#                 ###     my $stmt = $stmts->[0];
#                 ###     if (ref($stmt) eq 'Perlito5::AST::If' && $stmt->{otherwise} && $stmt->{otherwise}->isa('Perlito5::AST::Block')) {
#                 ###         # if ... else if ...
#                         my $stmts = $stmt->{otherwise}{stmts};
# 
#                         if ($stmts && @$stmts == 1) {
#                             my $stmt = $v->[0];
#                             $v->[0] = 
#                                 Perlito5::AST::Apply->new(
#                                     'arguments' => [
#                                         Perlito5::AST::Block->new(
#                                             'stmts' => [ $stmt ],
#                                         ),
#                                     ],
#                                     'code' => 'do',
#                                 );
#                         }
#                 ###     }
#                 ### }
#             }
#         }
#     }
# }
 
sub split_code_too_large {
    # work around Java "Code too large" error
    my @stmts = @_;
    # for my $stmt (@stmts) {
    #     split_deep_if($stmt);   # find deep nested ifs
    # }
    while (@stmts > 20) {
        # print STDERR "Code too large, split ", scalar(@stmts), " nodes\n";
        my @do = splice(@stmts, -15, 15);
        push @stmts,
            Perlito5::AST::Apply->new(
                'arguments' => [
                    Perlito5::AST::Block->new(
                        'stmts' => \@do,
                    ),
                ],
                'code' => 'do',
            );
    }
    return @stmts;
}


sub preprocess_regex {
    my $regex = shift;

    if ($regex->isa('Perlito5::AST::Apply') && $regex->{code} eq 'circumfix:<( )>') {
        # $x =~ ( ... )
        ($regex) = @{ $regex->{arguments} };
        return preprocess_regex($regex);
    }

    if ( $regex->isa('Perlito5::AST::Buf')   # $x =~ '\w'
      || $regex->isa('Perlito5::AST::Var')   # $x =~ $regex
      || ($regex->isa('Perlito5::AST::Apply') && $regex->{code} eq 'list:<.>')    # $x =~ ($r1 . $r2)
      )
    {
        $regex = Perlito5::AST::Apply->new(
            code      => 'p5:m',
            arguments => [ $regex, Perlito5::AST::Buf->new( buf => '' ) ]
        );
    }

    return $regex;
}

1;

=begin

=head1 NAME

Perlito5::Macro - Ast macros for Perlito

=head1 SYNOPSIS

    $ast = $ast.op_assign()

=head1 DESCRIPTION

This module implements some Ast transformations for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2011, 2012 by Flavio Soibelmann Glock.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

