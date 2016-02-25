use v5;
package Perlito5::Macro;
use strict;

{
package Perlito5::AST::Apply;
use strict;

my %op = (
    'infix:<+=>'  => 'infix:<+>',
    'infix:<-=>'  => 'infix:<->',
    'infix:<*=>'  => 'infix:<*>',
    'infix:</=>'  => 'infix:</>',
    'infix:<||=>' => 'infix:<||>',
    'infix:<&&=>' => 'infix:<&&>',
    'infix:<|=>'  => 'infix:<|>',
    'infix:<&=>'  => 'infix:<&>',
    'infix:<//=>' => 'infix:<//>',
    'infix:<.=>'  => 'list:<.>',
    'infix:<x=>'  => 'infix:<x>',
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
# Transformation for implementing state variables (perldoc -f state) by
# enclosing the sub in a do block, which in turn contains lexical variables
# that corresponding to the state variables in use in the sub. Returns a `do`
# expr node enclosing the modified sub, or 0 if no statevars are found in $self.
sub maybe_rewrite_statevars {
    my ($self) = @_;
    my $block = $self->{block} || return 0;
    my @init_flags; # Holds the *names* of intialization flags for state vars.
    my @vars;
    for my $idx (0..$#{ $block->{stmts} }) {
        my $stmt = $block->{stmts}[$idx];

        # my ($expr, @path) = _find_state_expr($stmt, "{block}", "{stmts}", "[$idx]");
        # next unless ($expr);
        # my $strpath = join '->', @path;
        # say "Found an expr at path $strpath";
        # die 'hard';
        # my ($transformed, $var, $flagvar) = rewrite_state_expr($expr);
        # set_path_to($self, \@path, $transformed);


        my $ptr_expr = get_state_expr_ptr(\$stmt);
        my ($transformed, $var, $flagvar) = rewrite_state_expr($ptr_expr);

        # TODO: Should we be mutating the tree in place?
        #my $ptr = \$expr;
        #$$ptr = $transformed;
        push @vars, $var, $flagvar;
    }
    # A final mutation we need to do is to find all usages of the state vars
    # in the sub and change their `decl` to `my`.
    # Now return a `do` node containing the transformed (mutated) $self and
    # the flag variables and the state variables declared as plain `my` vars.
    (scalar @vars) ? Perlito5::AST::Apply->new(
                         code      => 'do',
                         namespace => $block->{namespace},
                         arguments => [Perlito5::AST::Block->new(
                             sig   => undef,
                             stmts => [
                                 myvar_declaration_stmt(@vars),
                                 $self,
                             ]
                         )],
                     )
                   : 0;
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
    return Perlito5::AST::Apply->new(
        code      => 'my',
        arguments => [@_],
    );
}

# Given a path in the AST (see sub traverse_path() for an example path),
# sets the resulting node to $value. This is equivalent to walking to the
# parent of the target node (the node at the end of the path) and setting
# the last fragment to $value. For example, the call,
#   
#   set_path_to($node, ["{arguments}", "[0]", "{block}", "{stmts}", "[1]"], #   $newnode)
#
# will essentially do this:
#   
#   $node->{arguments}[0]{block}{stmts}[1] = $newnode;
# 
# Why is this needed? In C++, for example, one could take a proper pointer
# to the target node and then do
#   
#   *ptr_target = newnode;
#
# In Perl, references don't work that way, and we can't take a reference to
# an inner node in an AST and set it by saying
#   
#   $$ptr_target = $newnode;
#
sub set_path_to {
    my ($node, $path, $value) = @_;
    my @path = @{ $path };
    my $end = pop @path;
    ($end) || die "Empty path";
    $node = traverse_path($node, \@path);
    my $acc;
    if (defined ($acc = lookup_fragment($end))) {
        $node->{$acc} = $value;
    } elsif (defined ($acc = index_fragment($end))) {
        $node->[$acc] = $value;
    } else {
        die "Cannot handle path fragment $_";
    }
}

# Given a path fragments in the AST, walk the path and return the result node.
# A path fragment is a string, and must be take one of the following forms:
#
#   "{key}"
#   "[idx]"
#
# For example, a path of ["{arguments}", "[0]", "{block}", "{stmts}", "[1]"]
# will essentially return 
#   
#   $node->{arguments}[0]{block}{stmts}[1]
#
# if possible. This does *not* check for the existence of keys or indices along
# the way.
#
sub traverse_path {
    my ($tree, $path) = @_;
    my $node = $tree;
    for (@{ $path }) {
        my $acc;
        if (defined ($acc = lookup_fragment($_))) {
            $node = $node->{$acc};
        } elsif (defined ($acc = index_fragment($_))) {
            $node = $node->[$acc];
        } else {
            die "Cannot handle path fragment $_";
        }
    }
    return $node;
}

sub lookup_fragment {
    return ($_[0] =~ /\{(.+)\}/) ? $1 : undef;
}

sub index_fragment {
    return ($_[0] =~ /\[(.+)\]/) ? $1 : undef;
}

sub rewrite_state_expr {
    my ($target) = @_;
    my ($decl, $rhs);
    if (ref $target eq 'Perlito5::AST::Apply') {
        ($decl, $rhs) = @{ $target->{arguments} }; 
    } elsif (ref $target eq 'Perlito5::AST::Decl') {
        $decl = $target;
    } else {
        die "Invalid node type for state variable transformation: " . (ref $target);
    }
    my $state_var = $decl->{var};
    # This is the lexical variable that we'll inject into the surrounding
    # do block.
    my $var = Perlito5::AST::Var->new(
        namespace => $state_var->{namespace},
        sigil     => $state_var->{sigil},
        name      => $state_var->{name},
        decl      => 'my',
    );
    # A label to ensure hygienic names for the initialization flag.
    my $label = Perlito5::get_label();
    # The flag variable which will go in to the surrounding `do` block.
    my $flagvar = Perlito5::AST::Var->new(
        name      => $var->{name} . "_inited_" . $label, # e.g., x_initied_tmp404
        sigil     => '$',
        namespace => '',
        decl      => 'my',
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
# Finds and returns a node with a state variable declaration. This can be one
# of two types, a Perlito5::AST::Decl or a Perlito5::AST::Apply representing
# assignment ('infix:<=>'). Returns undef if nothing is found.
sub find_state_expr {
    my ($stmt, $path) = @_;
    $path //= [];
    return ($stmt, $path) if (is_node_state_decl($stmt) ||
                              is_node_state_assignment($stmt));
    push @{ $path }, '{arguments}';
    # If not, look for embedded state exprs.
    my $idx = 0;
    for (@{ $stmt->{arguments} }) {
        push @{ $path }, $idx++;
        my ($found, $path) = find_state_expr($_);
        return ($found, $path) if ($found);
        pop @{ $path };
    }
    return undef;
}

sub _find_state_expr {
    my ($stmt, @path) = @_;
    return ($stmt, @path) if (is_node_state_decl($stmt) ||
                              is_node_state_assignment($stmt));
    if (exists $stmt->{arguments}) {
        for (0..$#{ $stmt->{arguments} }){
            my $arg = $stmt->{arguments}[$_];
            my ($found, @retpath) = _find_state_expr($arg, (@path, "{arguments}", "[$_]"));
            $found && return ($found, @retpath);
        }
    }
    if (exists $stmt->{stmts}) {
        for (0..$#{ $stmt->{stmts} }){
            my $child_stmt = $stmt->{stmts}[$_];
            my ($found, @retpath) = _find_state_expr($child_stmt, (@path, "{stmts}", "[$_]"));
            $found && return ($found, @retpath);
        }
    }
    return undef;
}


sub get_state_expr_ptr {
    my ($ptr_stmt) = @_;
    return $ptr_stmt
        if (is_node_state_decl($$ptr_stmt) ||
            is_node_state_assignment($$ptr_stmt));

    if (exists ${$ptr_stmt}->{arguments}) {
        for (0..$#{ ${$ptr_stmt}->{arguments} }) {
            my $ptr_arg = \${$ptr_stmt}->{arguments}[$_];
            my $found = get_state_expr_ptr($ptr_arg);
            return $found
                if ($found);
        }
    }

    if (exists ${$ptr_stmt}->{stmts}) {
        for (0..$#{ ${$ptr_stmt}->{stmts} }) {
            my $ptr_child_stmt = \${$ptr_stmt}->{stmts}[$_];
            my $found = get_state_expr_ptr($ptr_child_stmt);
            return $found
                if ($found);
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
                            Perlito5::AST::Var->new(
                                'name' => '_',
                                'namespace' => '',
                                'sigil' => '$',
                            ),
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

=begin

=head1 NAME

Perlito5::Macro - Ast macros for Perlito

=head1 SYNOPSIS

    $ast = $ast.op_assign()

=head1 DESCRIPTION

This module implements some Ast transformations for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2011, 2012 by Flavio Soibelmann Glock.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

