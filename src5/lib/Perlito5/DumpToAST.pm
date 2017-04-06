package Perlito5::DumpToAST;

use Perlito5::AST;
use strict;

# $ perl -Isrc5/lib -e ' use strict; use Perlito5::DumpToAST; use Data::Dumper; my $s = [ 1,2.1,{a=>4},undef,\6 ]; $s->[3]=$s->[2]; my $seen = {}; print Dumper Perlito5::DumpToAST::dump_to_ast( $s, $seen, "s" ); '

sub dump_to_ast {
    my ($obj, $seen, $pos) = @_;

    return Perlito5::AST::Apply->new(code => 'undef', arguments => []) if !defined $obj;

    my $ref = ref($obj);
    if (!$ref) {
        if ( 0+$obj eq $obj ) {
            return Perlito5::AST::Int->new(int => $obj) if int($obj) == $obj;
            return Perlito5::AST::Num->new(num => $obj);
        }
        return Perlito5::AST::Buf->new(buf => $obj);
    }

    my $as_string = "$obj";
    return $seen->{$as_string} if $seen->{$as_string};
    $seen->{$as_string} = $pos;
        
    if ($ref eq 'ARRAY') {
        my @out;
        for my $i ( 0 .. $#$obj ) {
            # TODO - move self-referencing outside the expression
            my $here = Perlito5::AST::Index::INDEX( $pos, Perlito5::AST::Int->new(int => $i) );
            push @out, dump_to_ast($obj->[$i], $seen, $here);
        }
        return Perlito5::AST::Apply->new(code => 'circumfix:<[ ]>', arguments => \@out);
    }
    elsif ($ref eq 'HASH') {
        my @out;
        for my $i ( sort keys %$obj ) {
            # TODO - move self-referencing outside the expression
            my $here = Perlito5::AST::Lookup::LOOKUP( $pos, Perlito5::AST::Buf->new(buf => $i) );
            push @out, Perlito5::AST::Apply->new(
                code => 'infix:<=>>',
                arguments => [
                    Perlito5::AST::Buf->new(buf => $i),
                    dump_to_ast($obj->{$i}, $seen, $here),
                ],
            );
        }
        return Perlito5::AST::Apply->new(code => 'circumfix:<{ }>', arguments => \@out);
    }
    elsif ($ref eq 'SCALAR' || $ref eq 'REF') {
            # TODO - move self-referencing outside the expression if needed
        my $here = Perlito5::AST::Apply->new(code => 'prefix:<$>', arguments => [$pos]);
        return Perlito5::AST::Apply->new(
            code => 'prefix:<\\>',
            arguments => [dump_to_ast($$obj, $seen, $here)]
        );
    }
    elsif ($ref eq 'CODE') {
        # get the closed variables - see 'Sub' in Perl5 emitter
        my $closure_flag = bless {}, "Perlito5::dump";
        my $captures = $obj->($closure_flag) // {};

        my @vars;
        my $ast;
        my $source;
        my $sub_name;
        my $package = $captures->{__PKG__};
        if ($package) {
            push @vars, # "package $package;"
                Perlito5::AST::Apply->new(
                    'code'      => 'package',
                    'namespace' => $package,
                    'arguments' => [],
                );
        }
        for my $var_id (sort keys %$captures) {
            next if $var_id eq "__PKG__";
            if ($var_id eq '__SUB__') {
                my $sub_id = $captures->{$var_id};
                $ast = $Perlito5::BEGIN_SUBS{$sub_id};

                $sub_name = $ast->{namespace} . "::" . $ast->{name}
                    if $ast->{name};

                # my @data = $ast->emit_perl5();
                # my $out = [];
                # Perlito5::Perl5::PrettyPrinter::pretty_print( \@data, 0, $out );
                $source = $ast;
            }
            else {
                my $var_ast = $Perlito5::BEGIN_LEXICALS{$var_id};
                push @vars, 
                    # 'my ' . $var . ' = ' . dump_to_ast_deref($captures->{$var_id}, $seen, $pos) . '; ';
                    Perlito5::AST::Apply->new(
                        code => 'infix:<=>',
                        arguments => [
                            Perlito5::AST::Decl->new(
                                'attributes' => [],
                                'decl' => 'my',
                                'type' => '',
                                'var' => $var_ast,
                            ),
                            dump_to_ast_deref($captures->{$var_id}, $seen, $pos),  # TODO - $pos should be global
                        ],
                    );
            }
        }
        # say "dump_to_ast: source [[ $source ]]";
        return Perlito5::AST::Apply->new(
            code => 'do',
            arguments => [
                Perlito5::AST::Block->new(
                    stmts => [
                        @vars,
                        $source,
                        # ( $sub_name
                        #   ? '\\&' . $sub_name    # return pointer to subroutine ??? TODO - check this
                        #   : ''
                        # ),
                    ],
                ),
            ],
        );
    }

    # TODO find out what kind of reference this is (ARRAY, HASH, ...)
    # local $@;
    # eval {
    #     my @data = @$obj;
    #     say "is array";
    #     return 'bless(' . "..." . ", '$ref')";
    # }
    # or eval {
    #     $@ = '';
    #     my %data = %$obj;
    #     say "is hash";
    #     return 'bless(' . "..." . ", '$ref')";
    # };
    # $@ = '';
    
    # assume it's a blessed HASH
    
    my @out;
    for my $i ( sort keys %$obj ) {
        my $here = Perlito5::AST::Lookup::LOOKUP( $pos, Perlito5::AST::Buf->new(buf => $i) );
        push @out, Perlito5::AST::Apply->new(
            code => 'infix:<=>>',
            arguments => [
                Perlito5::AST::Buf->new(buf => $i),
                dump_to_ast($obj->{$i}, $seen, $here),
            ],
        );
    }
    return Perlito5::AST::Apply->new(
        code => 'bless',
        arguments => [
            Perlito5::AST::Apply->new(code => 'circumfix:<{ }>', arguments => \@out),
            Perlito5::AST::Buf->new(buf => $ref),
        ],
    );
}

sub dump_to_ast_deref {
    my ($obj, $seen, $pos) = @_;
    my $ref = ref($obj);
    return dump_to_ast(@_) if !$ref;
    if ($ref eq 'ARRAY') {
        return '()' unless @$obj;
        my @out;
        for my $i ( 0 .. $#$obj ) {
            my $here = Perlito5::AST::Index::INDEX( $pos, Perlito5::AST::Int->new(int => $i) ); # TODO don't deref
            push @out, dump_to_ast($obj->[$i], $seen, $here);
        }
        return Perlito5::AST::Apply->new(code => 'circumfix:<( )>', arguments => \@out);
    }
    elsif ($ref eq 'HASH') {
        return '()' unless keys %$obj;
        my @out;
        for my $i ( sort keys %$obj ) {
            my $here = Perlito5::AST::Lookup::LOOKUP( $pos, Perlito5::AST::Buf->new(buf => $i) ); # TODO don't deref
            push @out, Perlito5::AST::Apply->new(
                code => 'infix:<=>>',
                arguments => [
                    Perlito5::AST::Buf->new(buf => $i),
                    dump_to_ast($obj->{$i}, $seen, $here),
                ],
            );
        }
        return Perlito5::AST::Apply->new(code => 'circumfix:<( )>', arguments => \@out);
    }
    elsif ($ref eq 'SCALAR' || $ref eq 'REF') {
        my $here = Perlito5::AST::Apply->new(code => 'prefix:<$>', arguments => [$pos]);  # TODO don't deref
        return dump_to_ast($$obj, $seen, $here);
    }
    return dump_to_ast($obj, $seen, $pos);
}

sub _collect_refs_inner {
    my ($obj, $tab, $seen, $pos) = @_;
    return if !defined $obj;
    my $ref = ref($obj);
    return if !$ref;
    my $as_string = "$obj";
    if ($seen->{$as_string}) {
        # push things that are shared between data structures
        # return if $main::SEEN_COUNT{$as_string};
        # push @main::REFS, $obj;
        # push @main::REFS, { assign => [ $pos, $seen->{$as_string} ] };
        # $main::SEEN_COUNT{$as_string}++;
        return;
    }
    $seen->{$as_string} = $pos;
    if ($ref eq 'ARRAY') {
        return '[]' unless @$obj;
        for my $i ( 0 .. $#$obj ) {
            my $here = $pos . '->[' . $i . ']';
            _collect_refs_inner($obj->[$i], $tab, $seen, $here);
        }
        push @main::REFS, $obj;
        return;
    }
    elsif ($ref eq 'HASH') {
        return '{}' unless keys %$obj;
        for my $i ( sort keys %$obj ) {
            my $here = $pos . '->{' . $i . '}';
            _collect_refs_inner($obj->{$i}, $tab, $seen, $here);
        }
        push @main::REFS, $obj;
        return;
    }
    elsif ($ref eq 'SCALAR' || $ref eq 'REF') {
        _collect_refs_inner($$obj, $tab, $seen, $pos);
        push @main::REFS, $obj;
        return;
    }
    elsif ($ref eq 'CODE') {
        # get the closed variables - see 'Sub' in Perl5 emitter
        my $closure_flag = bless {}, "Perlito5::dump";
        my $captures = $obj->($closure_flag) // {};
        $pos = "SUB";
        my $subs = { sub => $captures->{__SUB__}, var => [] };
        for my $var_id (sort keys %$captures) {
            next if $var_id eq "__PKG__";
            if ($var_id eq '__SUB__') {
            }
            else {
                _collect_refs_inner($captures->{$var_id}, $tab, $seen, $pos);
                push @main::REFS, $captures->{$var_id};
                push @{ $subs->{var} }, $var_id;
            }
        }
        push @main::REFS, $obj;
        push @main::SUBS, $subs;
        return;
    }
    # TODO find out what kind of reference this is (ARRAY, HASH, ...)
    # assume it's a blessed HASH
    for my $i ( sort keys %$obj ) {
        my $here = $pos . '->{' . $i . '}';
        _collect_refs_inner($obj->{$i}, $tab, $seen, $here);
    }
    push @main::REFS, $obj;
    return;
}

sub collect_refs {
    my $scope = shift() // $Perlito5::GLOBAL;
    my $seen = {};
    my $dumper_seen = {};
    my $tab = "";
    for my $name (sort keys %$scope) {
        my $sigil = substr($name, 0, 1);
        my $item = $scope->{$name};
        if (ref($item) eq 'Perlito5::AST::Sub' && $item->{name}) {
            next;
        }
        if (substr($name, 7, 1) lt 'A') {
            # encode special variable names like $main::" to ${'main::"'}
            $name = $sigil . '{' . Perlito5::Dumper::escape_string(substr($name,1)) . '}'
        }
        my $ast = $item->{ast};
        if (ref($ast) eq 'Perlito5::AST::Var' && $ast->{_decl} eq "our") {
            # "our" variables are lexical aliases; we want the original global variable name
            $name =
                  ($ast->{_real_sigil} || $ast->{sigil})
                . ($ast->{namespace} || $ast->{_namespace})
                . "::" . $ast->{name};
        }
        my $bareword = substr($name, 1);
        if (ref($ast) eq 'Perlito5::AST::Var' && $sigil eq '$') {
            my $value = ${$bareword};
            my $dump = _collect_refs_inner( $value, "  ", $dumper_seen, $name );
            next if $dump eq 'undef';
        }
        elsif (ref($ast) eq 'Perlito5::AST::Var' && $sigil eq '@') {
            my $value = \@{$bareword};
            my $dump = _collect_refs_inner( $value, "  ", $dumper_seen, '\\' . $name );
        }
        elsif (ref($ast) eq 'Perlito5::AST::Var' && $sigil eq '%') {
            my $value = \%{$bareword};
            my $dump = _collect_refs_inner( $value, "  ", $dumper_seen, '\\' . $name );
        }
        elsif (ref($ast) eq 'Perlito5::AST::Var' && $sigil eq '*') {
            # *mysub = sub {...}
            if (exists &{$bareword}) {
                my $sub = \&{$bareword};
                my $dump = _collect_refs_inner($sub, '  ', $dumper_seen, '\\&' . $bareword);
            }
            if (defined ${$bareword}) {
                my $sub = \${$bareword};
                my $dump = _collect_refs_inner($sub, '  ', $dumper_seen, '\\$' . $bareword);
            }
            if (@{$bareword}) {
                my $sub = \@{$bareword};
                my $dump = _collect_refs_inner($sub, '  ', $dumper_seen, '\\@' . $bareword);
            }
            if (keys %{$bareword}) {
                my $sub = \%{$bareword};
                my $dump = _collect_refs_inner($sub, '  ', $dumper_seen, '\\%' . $bareword);
            }
        }
    }
}

sub _dump_AST_from_scope {
    my ($name, $item, $vars, $dumper_seen,) = @_;
    @_ = ();    # don't dump @_

    my $sigil = substr($name, 0, 1);
    if (ref($item) eq 'Perlito5::AST::Sub' && $item->{name}) {
        # TODO
        # _dump_global($item, $seen, $dumper_seen, $vars, $tab);
        warn "# don't know how to initialize subroutine $name in BEGIN";
        return;
    }

    # TODO - emit lexicals

    if (substr($name, 7, 1) lt 'A') {
        # encode special variable names like $main::" to ${'main::"'}
        $name = $sigil . '{' . Perlito5::Dumper::escape_string(substr($name,1)) . '}'
    }
    my $ast = $item->{ast};
    if (ref($ast) eq 'Perlito5::AST::Var' && $ast->{_decl} eq "our") {
        # "our" variables are lexical aliases; we want the original global variable name
        $name =
              ($ast->{_real_sigil} || $ast->{sigil})
            . ($ast->{namespace} || $ast->{_namespace})
            . "::" . $ast->{name};
    }
    my $bareword = substr($name, 1);
    if (ref($ast) eq 'Perlito5::AST::Var' && $sigil eq '$') {
        my $value = ${$bareword};
        return if !defined($value);
        push @$vars, # "$name = ", $dump;
                Perlito5::AST::Apply->new(
                    code => 'infix:<=>',
                    arguments => [
                        $ast,
                        dump_to_ast( $value, $dumper_seen, $ast ),  #'$name ),
                    ],
                );
    }
    elsif (ref($ast) eq 'Perlito5::AST::Var' && $sigil eq '@') {
        my $value = \@{$bareword};
        push @$vars, # "*$bareword = ", $dump;
                Perlito5::AST::Apply->new(
                    code => 'infix:<=>',
                    arguments => [
                        Perlito5::AST::Var->new( %$ast, sigil => '*' ),
                        dump_to_ast( $value, $dumper_seen, $ast ),  #''\\' . $name ),
                    ],
                );
    }
    elsif (ref($ast) eq 'Perlito5::AST::Var' && $sigil eq '%') {
        my $value = \%{$bareword};
        push @$vars, # "*$bareword = ", $dump;
                Perlito5::AST::Apply->new(
                    code => 'infix:<=>',
                    arguments => [
                        Perlito5::AST::Var->new( %$ast, sigil => '*' ),
                        dump_to_ast( $value, $dumper_seen, $ast ),  #''\\' . $name ),
                    ],
                );
    }
    elsif (ref($ast) eq 'Perlito5::AST::Var' && $sigil eq '*') {
        # *mysub = sub {...}
        if (exists &{$bareword}) {
            my $value = \&{$bareword};
            push @$vars, # "*$bareword = ", $dump;
                    Perlito5::AST::Apply->new(
                        code => 'infix:<=>',
                        arguments => [
                            Perlito5::AST::Var->new( %$ast, sigil => '*' ),
                            dump_to_ast( $value, $dumper_seen, $ast ),  #''\\&' . $bareword ),
                        ],
                    );
        }
        if (defined ${$bareword}) {
            my $value = \${$bareword};
            push @$vars, # "*$bareword = ", $dump;
                    Perlito5::AST::Apply->new(
                        code => 'infix:<=>',
                        arguments => [
                            Perlito5::AST::Var->new( %$ast, sigil => '*' ),
                            dump_to_ast( $value, $dumper_seen, $ast ),  #''\\$' . $bareword ),
                        ],
                    );
        }
        if (@{$bareword}) {
            my $value = \@{$bareword};
            push @$vars, # "*$bareword = ", $dump;
                    Perlito5::AST::Apply->new(
                        code => 'infix:<=>',
                        arguments => [
                            Perlito5::AST::Var->new( %$ast, sigil => '*' ),
                            dump_to_ast( $value, $dumper_seen, $ast ),  #''\\@' . $bareword ),
                        ],
                    );
        }
        if (keys %{$bareword}) {
            my $value = \%{$bareword};
            push @$vars, # "*$bareword = ", $dump;
                    Perlito5::AST::Apply->new(
                        code => 'infix:<=>',
                        arguments => [
                            Perlito5::AST::Var->new( %$ast, sigil => '*' ),
                            dump_to_ast( $value, $dumper_seen, $ast ),  #'\\%' . $bareword ),
                        ],
                    );
        }

    }
    else {
        warn "# don't know how to initialize variable $name in BEGIN";
    }

}

sub push_AST_refs {
    my ($vars, $array, $value) = @_;

    if (ref($value) eq 'Perlito5::AST::Apply' && $value->{code} eq 'circumfix:<[ ]>') {
        push @$vars, Perlito5::AST::Apply::PUSH(
            $array,
            Perlito5::AST::Apply->new( %$value, arguments => [] ),
        );
        my $deref = Perlito5::AST::Index::INDEX($array, Perlito5::AST::Int->new(int => -1));
        for my $arg (@{$value->{arguments}}) {
            push_AST_refs($vars, $deref, $arg);
        }
        return;
    }

    if (ref($value) eq 'Perlito5::AST::Apply' && $value->{code} eq 'circumfix:<{ }>') {
        push @$vars, Perlito5::AST::Apply::PUSH(
            $array,
            Perlito5::AST::Apply->new( %$value, arguments => [] ),
        );
        for my $arg (@{$value->{arguments}}) {
            my $hash = Perlito5::AST::Lookup::LOOKUP($array, $arg->{arguments}[0]);
            push @$vars,
                Perlito5::AST::Apply->new(
                    code => 'infix:<=>',
                    arguments => [
                        $hash,
                        $arg->{arguments}[1],
                    ],
                );
        }
        return;
    }

    push @$vars, Perlito5::AST::Apply::PUSH(
        $array,
        $value,
    );
}

sub dump_to_AST_after_BEGIN {
    # return a structure with the global variable declarations
    # this is used to initialize the ahead-of-time program
    my $scope = shift() // $Perlito5::GLOBAL;
    my $vars = [];
    my $dumper_seen = {};
    my $tab = "";

    # collect references for later
    @main::REFS = ();
    collect_refs($scope);
    use Data::Dumper;
    # print STDERR "REFS ", Data::Dumper::Dumper(\@main::REFS);
    print STDERR "SUBS ", Data::Dumper::Dumper(\@main::SUBS);

    my $refs = [];
    _dump_AST_from_scope(
        '@main::REFS',
        { ast => Perlito5::AST::Var->new(name => 'REFS', namespace => 'main', sigil => '@') },
        $refs,
        $dumper_seen
    );
    # print STDERR "refs ", Data::Dumper::Dumper($refs->[0]{arguments}[1]{arguments});
    for my $ast ( @{ $refs->[0]{arguments}[1]{arguments} } ) {
        push_AST_refs(
            $vars,
            Perlito5::AST::Var->new(
                '_decl' => 'global',
                '_namespace' => 'main',
                'name' => 'REFS',
                'namespace' => '',
                'sigil' => '@',
            ),
            $ast,
        );
    }
    for my $name (sort keys %$scope) {
        my $item = $scope->{$name};
        _dump_AST_from_scope($name, $item, $vars, $dumper_seen);
    }
    return \@$vars;
}

1;

