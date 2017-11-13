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
        my $current_package = "main";
        for my $var_id (sort keys %$captures) {
            next if $var_id eq "__PKG__";
            next if $Perlito5::BEGIN_SCRATCHPAD{$var_id};   # variable captured at BEGIN
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
                my $var = $Perlito5::BEGIN_LEXICALS{$var_id};
                $var = Perlito5::AST::Var->new(
                    %$var,
                    sigil => $var->{_real_sigil} || $var->{sigil},
                );
                my $decl = $var->{_decl} || 'my';    # our / my
                if ($decl eq 'our') {
                    my $var_namespace = $var->{_namespace} || $var->{namespace};
                    if ($var_namespace ne $current_package) {
                        push @vars, # "package $package;"
                            Perlito5::AST::Apply->new(
                                'code'      => 'package',
                                'namespace' => $var_namespace,
                                'arguments' => [],
                            );
                        $current_package = $var_namespace;
                    }
                    push @vars, 
                        Perlito5::AST::Decl->new(
                            'attributes' => [],
                            'decl' => $decl,
                            'type' => '',
                            'var' => $var,
                        );
                }
                else {
                    push @vars, 
                        # 'my ' . $var . ' = ' . dump_to_ast_deref($captures->{$var_id}, $seen, $pos) . '; ';
                        Perlito5::AST::Apply->new(
                            code => 'infix:<=>',
                            arguments => [
                                Perlito5::AST::Decl->new(
                                    'attributes' => [],
                                    'decl' => $decl,
                                    'type' => '',
                                    'var' => $var,
                                ),
                                dump_to_ast_deref($captures->{$var_id}, $seen, $pos),  # TODO - $pos should be global
                            ],
                        );
                }
            }
        }
        if ($package ne $current_package) {
            push @vars, # "package $package;"
                Perlito5::AST::Apply->new(
                    'code'      => 'package',
                    'namespace' => $package,
                    'arguments' => [],
                );
            $current_package = $package;
        }
 
        # warn "dump_to_ast: source: ", Perlito5::Dumper::Dumper( $source );
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
    elsif ($ref eq 'Regexp') {
        my $regex = "$ref";   #  (?: ... )
        return Perlito5::AST::Apply->new(
            code => 'p5:qr',
            arguments => [
                Perlito5::AST::Buf->new( buf => $regex ), 
                Perlito5::AST::Buf->new( buf => "" ), 
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

1;

