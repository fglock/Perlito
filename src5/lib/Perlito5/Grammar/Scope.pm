package Perlito5::Grammar::Scope;

use Perlito5::AST;
use strict;

sub new {
    return { block => [] };
}

sub new_base_scope {
    return { block => [] }
}

sub create_new_compile_time_scope {
    my $new_scope = { block => [] };
    push @{ $Perlito5::SCOPE->{block} }, $new_scope;   # start new compile-time lexical scope
    $Perlito5::SCOPE_DEPTH++;
    # warn "ENTER ", $Perlito5::SCOPE_DEPTH, " $Perlito5::PKG_NAME \n";
    $Perlito5::SCOPE = $new_scope;
    $Perlito5::SCOPE->{hint_scalar} = $^H;
    $Perlito5::SCOPE->{hint_hash}   = { %^H };
    # print STDERR "create_new_compile_time_scope [ $^H ]\n";
}

sub end_compile_time_scope {
    # warn "EXIT  ", $Perlito5::SCOPE_DEPTH, "\n";
    $^H = $Perlito5::SCOPE->{hint_scalar};
    %^H = %{ $Perlito5::SCOPE->{hint_hash} || {} };
    my $pos = 0;
    $Perlito5::SCOPE_DEPTH--;
    $Perlito5::SCOPE = $Perlito5::BASE_SCOPE;
    while ($Perlito5::SCOPE_DEPTH > $pos) {
        $pos++;
        $Perlito5::SCOPE = $Perlito5::SCOPE->{block}[-1]; 
    }
    # print STDERR "end_compile_time_scope [ $^H ]\n";
}

sub compile_time_glob_set {
    # set a GLOB at compile-time
    my ($glob, $value, $namespace) = @_;
    if ( !ref($glob) ) {
        if ( $glob !~ /::/ ) {
            $glob = $namespace . '::' . $glob;
        }
        # mark the variable as "seen"
        my @parts = split "::", $glob;
        my $name = pop @parts;
        Perlito5::AST::Var->new( name => $name, namespace => join("::", @parts), sigil => "*", _decl => "global" );

        if (ref($value) eq 'SCALAR') {
            $Perlito5::VARS{'$' . $glob} = 1;
        }
        if (ref($value) eq 'HASH') {
            $Perlito5::VARS{'%' . $glob} = 1;
        }
        if (ref($value) eq 'ARRAY') {
            $Perlito5::VARS{'@' . $glob} = 1;
        }
        if (ref($value) eq 'CODE') {
            if (!exists( $Perlito5::PROTO->{$glob} )) {
                $Perlito5::PROTO->{$glob} = undef;
            }
        }
    }
    *{$glob} = $value;
}

sub lookup_variable {
    # search for a variable declaration in the compile-time scope
    my $var = shift;
    my $scope = shift() // $Perlito5::BASE_SCOPE;

    return $var if $var->{namespace};       # global variable
    return $var if $var->{_decl};           # predeclared variable

    my $look = lookup_variable_inner($var, $scope, 0);
    return $look if $look;

    return if ref($var) ne 'Perlito5::AST::Var';

    my $c = substr($var->{name}, 0, 1);
    if ( $var->is_special_var() ) {
        # special variable
        $var->{_decl} = 'global';
        $var->{_namespace} = 'main';
        return $var;
    }

    if ( $var->{sigil} eq '$' && ( $var->{name} eq 'a' || $var->{name} eq 'b' ) ) {
        if ( !$var->{_real_sigil} ) {
            # special variables $a and $b
            $var->{_decl} = 'global';
            $var->{_namespace} = $Perlito5::PKG_NAME;
            return $var;
        }
    }
    return;
}


sub lookup_variable_inner {
    # search for a variable declaration in the compile-time scope
    my ($var, $scope, $depth) = @_;

    # warn "depth $depth ", $Perlito5::SCOPE_DEPTH, "\n";
    return if $depth > $Perlito5::SCOPE_DEPTH;

    my $block = $scope->{block};
    if ( @$block && ref($block->[-1]) eq 'HASH' && $block->[-1]{block} ) {
        # lookup in the inner scope first
        my $look = lookup_variable_inner($var, $block->[-1], $depth + 1);
        return $look if $look;
    }

    if ( ($scope->{compacted} + 100) < @$block ) {
        # garbage-collect the scope
        my %seen;
        my @out;
        my $start = $#$block - 500;
        $start = 1 if $start < 1;
        for my $i ($start .. $#{$block}) {
            my $item = $block->[$i];
            my $s = join(':', map {;
                $_ . '=' . $item->{$_}
            } sort {;
                $a cmp $b
            } keys(%{$item}));
            $seen{$s}++ || push(@out, $item)
        }
        # print STDERR:: 'block ', scalar(@{$block}), ' ', scalar(@out), ' ', $scope->{compacted}, "\n";
        $scope->{'block'} = [ @{$block}[ 0 .. $start - 1 ], @out ];
        $scope->{compacted} += 100;
    }

    for my $item (reverse @$block) {
        if (ref($item) eq 'Perlito5::AST::Var' && $item->{_decl}
            && $item->{_decl} ne 'global'
            && $item->{name} eq $var->{name}) {
            my $sigil = $var->{_real_sigil} || $var->{sigil};
            my $item_sigil = $item->{_real_sigil} || $item->{sigil};
            # TODO - namespace
            # TODO - $a[10]  $#a  ${"a"}
            # TODO - check "strict"
            if ($sigil eq $item_sigil) {
                # print "found name: $item->{sigil} $item->{name} decl: $item->{_decl}\n";
                return $item;
            }
        }
    }
    return;
}

sub check_variable_declarations {
    # examine the variables that were used in the last statement
    # - check if the variables were declared
    # - insert the variables in the current compile-time scope

    # print "env: ", Data::Dumper::Dumper( $Perlito5::SCOPE->{block} );
    for my $item ( @Perlito5::SCOPE_STMT ) {
        if (ref($item) eq 'Perlito5::AST::Var') {
            my $var = $item;
            my $look = lookup_variable($var);
            if ($look) {
                $var->{_id} = $look->{_id} if $look->{_id};
                $var->{_decl} = $look->{_decl} if $look->{_decl};
                $var->{_namespace} = $look->{_namespace} if $look->{_namespace};
            }
            else {
                # unknown variable
                if ( $^H & $Perlito5::STRICT_VARS ) {
                    # warn "look: ", Data::Dumper::Dumper(\@Perlito5::SCOPE_STMT);
                    my $sigil = $var->{_real_sigil} || $var->{sigil};
                    if ($sigil ne '*' && $sigil ne '&') {
                        if (length($var->{name}) >= 2 && substr($var->{name}, -2) eq '::') {
                            # looks like a symbol table reference
                        }
                        elsif ($Perlito5::VARS{ $sigil . $Perlito5::PKG_NAME . '::' . $var->{name} }) {
                            # "use vars"
                        }
                        else {
                            Perlito5::Compiler::error( 'Global symbol "' . $sigil . $var->{name} . '"'
                                . ' requires explicit package name' );
                        }
                    }
                }
                $var->{_decl} = 'global';
                $var->{_namespace} = $Perlito5::PKG_NAME;
            }
            if ($var->{name} && ($var->{namespace} || $var->{_namespace})) {
                my $compiletime_name;
                if ($var->{'name'} lt 'A' || $var->{'name'} eq '\\') {
                    $compiletime_name =
                          ($var->{_real_sigil} || $var->{sigil})
                        . $var->{name};
                }
                else {
                    $compiletime_name =
                          ($var->{_real_sigil} || $var->{sigil})
                        . ($var->{namespace} || $var->{_namespace})
                        . "::" . $var->{name}
                        . ($var->{_decl} eq "global" ? "" : $var->{_id} ? "_" . $var->{_id} : "");
                }
                $Perlito5::GLOBAL->{$compiletime_name} = { value => undef, ast => $var };
            }
        }
    }
    push @{ $Perlito5::SCOPE->{block} }, @Perlito5::SCOPE_STMT;
    @Perlito5::SCOPE_STMT = ();
}

sub get_snapshot {
    # return a structure with the variable declarations in the current compile-time scope
    # this is used by eval-string at runtime
    my @result;
    my $scope = shift() // $Perlito5::BASE_SCOPE;
    my $block = $scope->{block};
    if ( @$block && ref($block->[-1]) eq 'HASH' && $block->[-1]{block} ) {
        # lookup in the inner scope first
        my $look = get_snapshot($block->[-1]);
        unshift @result, @{ $look->{block} };
    }
    for my $item (@$block) {
        if (   ref($item) eq 'Perlito5::AST::Var'
            && $item->{_decl}
            && $item->{_decl} ne "global"
            && $item->{_decl} ne "local" )
        {
            unshift @result, $item;
        }
    }
    return { block => \@result };
}

1;

