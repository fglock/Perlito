# pX/Common/iterator_engine_p6compiler.pl - fglock
#
# experimental implementation of a p6 compiler
# 

use strict;
use warnings;

require 'iterator_engine_p6rule.pl';

# XXX - make grammars inherit from Grammar; make grammars inheritable
# XXX - write an emitter that generates perl5 regexes (or dies)
# XXX - add (API/documentation) to generate unnamed rules, unnamed grammars
# XXX - fix the extra commas in the generated code
# XXX - create error messages for compiling errors

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Pad = '# ';

{
    package grammar1;

    no warnings 'once';
    use vars qw( @statements @terms @ops );

    # bootstrap the 'grammar' syntax

    my  $immediate_statement_precompiled = ::compile_rule( q(
            <?ws>? <@grammar1::statements> <?ws>?
        ), {print_ast=>0} );
        
    *immediate_statement_rule = sub {
        #print "immediate_statement: parse ###$_[0]###\n";
        my $match = $immediate_statement_precompiled->( @_ );
        #print "immediate_statement: BEGIN AST: \n", Dumper( $match->{capture} );
        return $match;
    };

    *immediate_statement_exec = sub {
        my $match = immediate_statement_rule( @_ );
        # print "immediate_statement_exec: BEGIN AST: \n", Dumper( $match->{capture} );
        return $match unless $match->{bool};
        # print "immediate_statement_exec: BEGIN AST: \n", Dumper( $match->{capture} );
        my $program = Perl6Grammar::emit( $match->{capture} );
        #print "immediate_statement_exec: matching ###$_[0]###\n";
        #print "immediate_statement_exec: eval'ing code:\n###$program###\n";
        no strict 'refs';
        my $code = eval($program);
        #print "Error in statement:\n", $program if $@;
        die "error in immediate_statement_exec: " . $@
            if $@;
        # print "immediate_statement_exec: CODE[ $code ]\n";
        return {
            %$match,
            capture => [ { perl5 => $program } ],
        }
    };


    *grammar = ::compile_rule( <<'__p6__' ); 
        <immediate_statement_exec>*
__p6__


    *rule_decl = ::compile_rule( <<'__p6__' );
        rule <ws> <ident> <ws>? \{ <rule> \}  
            { return { rule_decl => $() ,} }
__p6__


    push @grammar1::statements, \&rule_decl;
    # done bootstrapping!
    
    # improve the grammar a little:
    # - 'grammar' declaration
    # - 'push' - add terms into the grammar tables
    Perl6Grammar::compile( <<'__p6__' , {print_ast=>0} );
        rule grammar1::grammar_name { 
            grammar <ws> <ident> <ws>? \;
                { return { grammar_name => $() ,} }
        }
        rule grammar1::_push {
            $op := (push|unshift) <ws> <variable> <ws>? \, <ws>? $code := (.*?) <ws>? \;
                { return { _push => $() ,} }
        }
__p6__
    push @grammar1::statements, \&grammar_name;
    push @grammar1::statements, \&_push;
    
    # the remaining grammar can be written using itself

    # load/precompile Prelude

    my $prelude_file = 'iterator_engine_p6prelude';
    my $recompile;
    if ( -f "$prelude_file-cached.pl" ) {
        $recompile = 
            -M "$prelude_file-cached.pl" > 
            -M "$prelude_file.p6";
    }
    else {
        $recompile = 1;
    }
    if ( $recompile ) {
        local $/ = undef; 
        print "* precompiling Prelude: $prelude_file.p6\n";
        open( FILE, "<", "$prelude_file.p6" ) or 
            die "can't open prelude file: $prelude_file.p6 - $!";
        my $prelude = <FILE>;
        # print "Prelude:$prelude\n";
        my $perl5 = Perl6Grammar::compile( $prelude );
        # print "MATCH\n", Dumper($match), "END MATCH\n";
        print "* caching Prelude: $prelude_file-cached.pl\n";
        open( FILE, ">", "$prelude_file-cached.pl" ) or
            die "can't open prelude file: $prelude_file-cached.pl - $!";
        print FILE "# generated file - do not edit!\n" . $perl5;
        close FILE;
    }
    else {
        print "* loading Prelude: $prelude_file-cached.pl\n";
        require "$prelude_file-cached.pl";
    }


    {
        my $filename = shift || die "no filename";
        local $/ = undef; 
        print "* compiling: $filename\n\n";
        open( FILE, "<", $filename ) or 
            die "can't open file: $filename - $!";
        my $source = <FILE>;
        my $perl5 = Perl6Grammar::compile( $source ); 
        # , { print_ast => 1 } );
        # print "MATCH\n", Dumper($match), "END MATCH\n";
    }

    exit;

=for later
        rule sub_application {
            <@grammar1::terms> <ws>? <@grammar1::ops> <ws>? <@grammar1::terms>
        }
        push @terms, \&sub_application;
    
        # XXX - this doesn't work
        #       say sub { print 'anonymous sub'; } ;
        rule anon_sub {
            sub <block>
                { return { anon_sub => $<block> ,} }
        }
        push @terms, \&anon_sub;

        rule assignment {
            $lvalue := (<variable>) <ws>? \= <ws>? $rvalue := (<variable>) <ws>? \;
                { return { assignment => [ $<lvalue>, $<rvalue> ] ,} }
        }
        unshift @terms, \&assignment;
        rule eval_perl5 {
            eval <ws>? \( <ws>? \" <code> \" <ws>? \, <ws>? \:lang\<perl5\> <ws>? \) <ws>? \;
        }
    # sub print ... 
    # sub 'Y' - is assoc 'list'
    # sub infix:<+> { eval( '$_[0]+$_[1]', :lang<perl5> ) }

    #        print '1' + '1';
    #    $a = $b;
    # TODO - $a = $b; - see 'rule assignment' above
    # TODO - rule comment { \# .*? [<newline>|$$] }
=cut

}

# ------ emitter

my $namespace = 'grammar1::';

{
  package Perl6Grammar;
  use Data::Dumper; 

sub header {
    return <<EOT;
#! perl
#
# grammar file
# perl5 code generated by iterator_engine_p6grammar.pl - fglock

use strict;
use warnings;
require 'iterator_engine.pl';
require 'iterator_engine_p6rule_lib.pl';

EOT
}

# compile( $source, {flag=>value} );
#
# flags:
#   print_program=>1 - prints the generated program
#
sub compile {
    #print "compile: matching: \n$_[0]\n";
    my $match = grammar1::grammar->( $_[0] );
    #print "compile: matched.\n";
    my $flags = $_[1];
    die "compile: syntax error in program '$_[0]' at '" . $match->{tail} . "'\n"
        if $match->{tail};
    die "compile: syntax error in program '$_[0]'\n"
        unless $match->{bool};
    print "compile: generated ast:\n", Dumper( $match->{capture} ) if $flags->{print_ast};
    my $program = emit( $match->{capture} );
    print "compile: generated code:\n$program" if $flags->{print_program};
    return $program;

}

sub get_data {
    match::get( { capture => $_[0] }, $_[1] ) 
}
sub get_str {
    match::str( get_data( @_ ) )
}

# XXX not used - intended to bind variables in a macro, when it returns an AST
#     instead of string
sub bind_variable {
    my $n = $_[0];
    my ( $var_name, $value ) = @_;
    #print ref($n),"\n";
    if ( ref( $n ) eq 'ARRAY' ) {
        bind_variable( $_, @_[1,2] ) for @$n;
    }
    elsif ( ref( $n ) eq 'HASH' ) 
    {
        #print Dumper($n);
        my ( $k ) = keys %$n;
        my $v = $n->{$k};
        #print "*** $k, $v \n";
        #return unless defined $k;
        return bind_variable( $v, @_[1,2] ) if ref( $v );
        if ( $k eq 'variable' && $v eq $_[1] ) {
            #print "subst $k $v @_[1,2] ",$_[0]->{$k}, "\n";
            $_[0]->{$k} = $_[2];
        }
    }
}

sub emit 
{
    my $n = $_[0];
    # local $Data::Dumper::Indent = 0;
    # print "emit: ", ref($n)," ",Dumper( $n ), "\n";

    # $n = $n->{match};

    if ( ! defined $n || ref($n) eq '' ) {
        # empty node; maybe a <null> match
        return '';
    }
    if ( ref( $n ) eq 'ARRAY' ) {
        my @s;
        for ( @$n ) {
            push @s, emit( $_ );
        }
        return join( '', @s ) ;
    }
    elsif ( ref( $n ) eq 'HASH' ) 
    {
        my ( $k ) = keys %$n;
        my $v = $n->{$k};
        return '' unless defined $k;
        #print "$k => $v \n";
    
        if ( $k eq 'pod' ) {
            return '';
        }
        if ( $k eq 'ws' ) {
            return '';
        }
        if ( $k eq 'grammar_name' ) {
            my $ident = get_str( $v, '$<ident>' );
            return "package $ident;\n";
        }
        if ( $k eq 'rule_decl' ) {
            my $name = get_str( $v, '$<ident>' );
            my $program = main::emit_rule( get_data( $v, '$<rule>' ), '' );
            return "*{'$name'} = \n$program;\n";
        }
        if ( $k eq 'block' ) {
            return "    {\n" . emit($v) . "    }\n";
        }
        if ( $k eq 'sub_decl' ) {
            my $fix =   get_str( $v, '$<fix>' );
            my $id =    get_str( $v, '$<id>' );
            my $block = get_data( $v, '$<block>' );
            # XXX - register fixity in grammar
            return 
                # "    { no strict 'refs';\n" .
                "    *{'$fix:<$id>'} = sub\n" . emit($block) . "    ;\n" .
                # "    }\n" .
                "    push \@grammar1::ops, ::compile_rule( '" .
                    quotemeta( $fix . ':<' . $id . '>' ) . "' );\n";
        }
        if ( $k eq 'sub_application' ) {
            #print 'sub_application', Dumper($v); 
            my $term1 = emit( get_data( $v, '$<term1>' ) );
            my $op =    get_str( $v, '$<op>' );
            my $term2 = emit( get_data( $v, '$<term2>' ) );
            return 
                "    &{'$op'} ( $term1, $term2 );\n";
        }
        if ( $k eq 'sub_application_term' ) {
            #print 'sub_application', Dumper($v); 
            my $term1 = emit( get_data( $v, '$<term1>' ) );
            my $op =    get_str( $v, '$<op>' );
            my $term2 = emit( get_data( $v, '$<term2>' ) );
            return 
                "    &{'$op'} ( $term1, $term2 )\n";
        }
        if ( $k eq '_push' ) {
            my $op =   get_str( $v, '$<op>' );
            my $name = get_str( $v, '$<variable>' );
            my $code = get_str( $v, '$<code>' );
            return "    $op $name, $code;\n";
        }
        if ( $k eq '_simple_statement' ) {
            my $op = get_str( $v, '$<op>' );
            $op = 'die "not implemented"' if $op eq '...';
            return "    $op;\n";
        }
        if ( $k eq '_my' ) {
            my $op =   get_str( $v, '$<op>' );
            my $name = get_str( $v, '$<variable>' );
            return "    $op $name;\n";
        }
        if ( $k eq '_return' ) {
            # print "return: ", Dumper($v);
            my $val = emit( get_data( $v, '$<val>' ) );
            return "    return $val;\n";
        }
        if ( $k eq '_print' ) {
            my $op =   get_str( $v, '$<op>' );
            my $list = get_data( $v, '$<list>' );
            my $cmd = "    print";
            $cmd =    "    warn" if $op eq 'warn';
            my $s;
            for ( @$list ) {
                next unless ref($_) eq 'HASH';
                my $s1 = emit($_);
                $s .= "$cmd $s1;\n"
                    if $s1;
            }
            return $s . "$cmd \"\\n\";\n" 
                if $op eq 'say';
            return $s;
        }
        if ( $k eq 'term1' || $k eq 'term2' ) {
            #print Dumper($v);
            return $v unless ref($v);
            return emit( $v->[0]);
        }
        if ( $k eq 'literal' ) {
            # $v =~ s/([\'])/\\$1/g;
            return "'" . $v . "'";
            # return '"' . quotemeta($v) . '"';
            #$v =~ s/(["\$%@])/\\$1/g;
            #return '"' . $v . '"';
        }
        if ( $k eq 'eval_perl5' ) {
            # print "eval_perl5: $v\n";
            my $res = eval emit($v);
            print "Error in eval_perl5:\n", $v if $@;
            die $@ if $@;
            return $res;
        }
        if ( $k eq 'variable' ) {
            # print "variable ", Dumper( $v );
            return $v;
        }
        if ( $k eq 'immediate_statement_exec' ) {
            return get_data( $v, '$<perl5>' );
        }
        if ( $k eq 'macro' ) {
            
            #print Dumper( get_data( $v, "\$()" ) );
            
            my ($prefix, $id, $list, $rule, $block) = 
                map { get_data( $v, "\$<$_>" ) } 
                qw( prefix id list rule code );
            $prefix = match::str( $prefix );
            $id     = match::str( $id );
            $block  = match::str( $block );
            my @args;
            for ( @$list ) {
                next unless ref($_) eq 'HASH';
                push @args, match::str( $_ );  # emit($_);
            }
            # no parameters? look into the regex
            unless ( @args ) {
                my %h;
                for ( @$rule ) {
                    next unless ref($_) eq 'HASH';
                    next unless exists $_->{named_capture};
                    
                    for ( @{ $_->{named_capture} } ) {
                        next unless ref($_) eq 'HASH';
                        # XXX - if there are multiple definitions, it is an array
                        my $name = '$' . match::str( $_ );  
                        $h{$name} = 1;
                        last;
                    }
                }
                @args = keys %h;
            } 

            # my $rule_code = main::emit_rule( $rule, '' );
            # print "macro: $prefix / $id \n";  #, Dumper($list);
            # print "macro: args = @args\n";
            # print "macro: rule = \n$rule_code\n";
            # print "macro: block = \n", match::str($block),"\n";
    
            # XXX don't use source filter: variable substitutions $() in the body AST
    
            # XXX this algorithm depends on variable declaration 
            #     like:  ( $a ) is parsed ( / $a := (.*?) / )
    
            my $binding = '';
            for ( 0 .. $#args ) {
                my $ident = $args[$_];
                $ident =~ s/^.//;  # no sigil
                $binding .= 
                    #"    print \"binding: \\n\" . Dumper( match::get( \$_[0], '\$<$ident>' ) );\n" .
                    "    my $args[$_] = " . 
                    "match::str( match::get( \$_[0], '\$<$ident>' ) );\n" .
                    # "    \$src =~ s/\\\\\\$args[$_]/\\$args[$_]/g; \n" .
                    "    $args[$_] =~ s/([\\'\\\\])/\\\\\$1/g;\n" .
                    #"    print \"bound: \\$args[$_] $args[$_] \$src \"; " .
                    "\n";
            }
            # print "macro: var binding: \n$binding";
    
            # emit the rule
            local $Data::Dumper::Pad = '    ' x 2;
            local $Data::Dumper::Terse = 1;
            my $res = 
    
                "*{'$prefix:<$id>'} = sub {\n" .
                "    my \$rule = ruleop::concat( \n" .
                "        ruleop::constant( '$prefix:<$id>' ),\n" .
                "        \\&grammar1::ws_star,\n" .
                main::emit_rule( $rule ) .
                "    );\n" .
    
                #"    my \$body_ast = \n" .
                #Data::Dumper->Dump( $block ) .
                #"    ;\n" .
    
                "    my \$match = \$rule->( \@_ );\n" .
                "    return unless \$match;\n" .
                "    my \$code = sub { \n" .
                # "    print 'matched: ', Dumper( \$_[0]->{capture} ); \n" .
                "    my \$src = <<\'!EOT!\'; \n" . 
                $block . 
                "\n!EOT!\n" .
                $binding .
                #"    print 'eval: ', \$a, '  ', \$src; \n" .
                "    \$src =~ s/([\\'\"\\\\])/\\\\\$1/g;\n" .
                "    my \$ret = eval( '\"' . \$src . '\"' ); \n" .
                #"    print \"Error in macro eval:\n\", \$src if \$\@; \n" .
                "    die \$@ if \$\@; \n" .
                #"    \$ret =~ s/([\\'\"])/\\\\\$1/g;\n" .
                #"    print \"ret: ###\$ret### \\n\"; \n" .
                "    my \$ast = grammar1::immediate_statement_rule( \$ret );\n" .
                "    die \"compile: syntax error in macro at '\" . \$ast->{tail} . \"'\\n\"\n" .
                "        if \$ast->{tail};\n" .
                #"    print \"ast: \\n\", Dumper( \$ast->{capture} ); \n" .
                "    my \$perl5 = Perl6Grammar::emit( \$ast->{capture} );\n" .
                #"    print \"perl5: ###\$perl5### \\n\"; \n" .
                "    my \$expanded = eval \$perl5;\n" .
                #"    print \"Error in expanded macro eval:\n\", \$perl5 if \$\@; \n" .
                "    die \$@ if \$\@; \n" .
                #"    print \"expanded: ###\$expanded### \\n\"; \n" .
                "    my \$final_ast = \n" .
                "        ::compile_rule( q( [ <?ws>? <\@grammar1::statements> ]* <?ws>? ) )\n" .
                "        ->( \$expanded );\n" .
                "    die \"compile: syntax error in macro at '\" . \$final_ast->{tail} . \"'\\n\"\n" .
                "        if \$final_ast->{tail};\n" .
                #"    print \"final ast: \\n\", Dumper(\$final_ast->{capture}); \n" .
                "    return \$final_ast;\n" .
                #"    my \$perl5_final = Perl6Grammar::emit( \$final_ast->{capture} );\n" .
                #"    print \"perl5_final: ###\$perl5_final### \\n\"; \n" .
                #"    return \$perl5_final;\n" .
                "    };\n" .
                "    my \$ast = \$code->( \$match ); \n" .
                #"    print \"tail: \", \$match->{tail}, \"\\n\"; " .
                "    return { \%\$match, capture => [ \$ast->{capture} ] }; \n" .
                "};\n";
    
            # register new syntax in the grammar category
    
            # example: macro statement_control:<if> ($expr, &ifblock) {...}
            # XXX - this is very rough
            my $category = $prefix;
            $category = 'statements' if $prefix eq 'statement_control';
    
            $res .= "    push \@grammar1::$category, \\&{'$prefix:<$id>'};\n";
    
            #print "macro: expanded:\n$res";
            return $res;
        }
        die "unknown node in emitter: $k";
    }
    die "unknown node: ", Dumper( $n );
}

} # /package

1;
