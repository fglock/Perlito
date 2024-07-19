use strict;
use warnings;

# Environment to store variable values and subroutines
my %environment = (
    vars => {},
    subs => {},
);

# Interpreter function
sub interpret {
    my ($node, $local_env) = @_;
    
    $local_env //= {};

    return unless $node;

    if ($node->{type} eq 'literal') {
        return $node->{value};
    }
    elsif ($node->{type} eq 'variable') {
        return $local_env->{$node->{name}} // $environment{vars}{$node->{name}};
    }
    elsif ($node->{type} eq 'assign') {
        my $value = interpret($node->{value}, $local_env);
        $environment{vars}{$node->{name}} = $value;
        return $value;
    }
    elsif ($node->{type} eq 'binary') {
        my $left = interpret($node->{left}, $local_env);
        my $right = interpret($node->{right}, $local_env);
        if ($node->{operator} eq '+') {
            return $left + $right;
        }
        elsif ($node->{operator} eq '-') {
            return $left - $right;
        }
        elsif ($node->{operator} eq '*') {
            return $left * $right;
        }
        elsif ($node->{operator} eq '/') {
            return $left / $right;
        }
    }
    elsif ($node->{type} eq 'if') {
        if (interpret($node->{condition}, $local_env)) {
            return interpret($node->{then}, $local_env);
        } else {
            return interpret($node->{else}, $local_env);
        }
    }
    elsif ($node->{type} eq 'while') {
        while (interpret($node->{condition}, $local_env)) {
            interpret($_, $local_env) for @{$node->{body}};
        }
        return;
    }
    elsif ($node->{type} eq 'call') {
        my $sub = $environment{subs}{$node->{name}};
        my @args = map { interpret($_, $local_env) } @{$node->{args}};
        return $sub->(@args);
    }
    elsif ($node->{type} eq 'sub') {
        $environment{subs}{$node->{name}} = sub {
            my @args = @_;
            my %new_local_env = %$local_env;
            @new_local_env{@{$node->{params}}} = @args;
            interpret($_, \%new_local_env) for @{$node->{body}};
        };
        return;
    }
}

# Example usage

# Define a subroutine in the syntax tree
my $syntax_tree = [
    { type => 'sub', name => 'add', params => ['a', 'b'], body => [
        { type => 'return', value => { type => 'binary', operator => '+', left => { type => 'variable', name => 'a' }, right => { type => 'variable', name => 'b' } } }
    ] },
    
    # Call the subroutine
    { type => 'assign', name => 'result', value => { type => 'call', name => 'add', args => [
        { type => 'literal', value => 3 },
        { type => 'literal', value => 4 }
    ] } }
];

# Interpret the syntax tree
interpret($_) for @$syntax_tree;

# Print the result
print "result = $environment{vars}{result}\n"; # Output: result = 7
