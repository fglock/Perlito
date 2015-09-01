#
# $ perl -Isrc5/lib misc/generate.pl 
#

use Perlito5::Perl5::Emitter;
use Perlito5::Perl5::PrettyPrinter;

sub r { int( sqrt( rand(5) ) ) }

my @v = qw/ a b x self /;
my @bool = qw{ infix:<&&> infix:<&&> infix:<||> infix:<||> infix:<//> };
my @oper = qw{ infix:<+> infix:<-> infix:<*> infix:</> };
my @compare = qw{ infix:<==> infix:<>=> infix:<<=> infix:<>> infix:<<> };

sub gen_ident {
    return $v[ rand(@v) ];
}

sub gen_var {
    return Perlito5::AST::Var->new( sigil => '$', name => gen_ident(), _decl => 'my' );
}

sub gen_bool {
    my $r = rand();
    if ( $r > 0.5 ) {
        return Perlito5::AST::Apply->new(
            code => $bool[ rand(@bool) ],
            arguments => [ gen_compare(), gen_compare() ],
        );
    }
    return gen_var();
}

sub gen_compare {
    my $r = rand();
    if ( $r > 0.5 ) {
        return Perlito5::AST::Apply->new(
            code => $compare[ rand(@compare) ],
            arguments => [ gen_exp(), gen_exp() ],
        );
    }
    return gen_exp();
}

sub gen_exp {
    my $r = rand();
    if ( $r > 0.8 ) {
        return Perlito5::AST::Apply->new(
            code => $oper[ rand(@oper) ],
            arguments => [ gen_exp(), gen_exp() ],
        );
    }
    if ( $r > 0.7 ) {
        return Perlito5::AST::Apply->new(
            code => "infix:<=>",
            arguments => [ gen_var(), gen_exp() ],
        );
    }
    return gen_var();
}

sub gen_stmt {
    my $r = rand();
    if ( $r > 0.7 ) {
        return Perlito5::AST::If->new(
            cond => gen_bool(),
            body => gen_block(),
        );
    }
    if ( $r > 0.6 ) {
        return Perlito5::AST::While->new(
            cond => gen_bool(),
            body => gen_block(),
        );
    }
    if ( $r > 0.4 ) {
        return Perlito5::AST::Apply->new(
            code => "infix:<=>",
            arguments => [ gen_var(), gen_exp() ],
        );
    }
    return gen_exp;
}

sub gen_block {
    return Perlito5::AST::Block->new( stmts => [ map { gen_stmt() } 0 .. r() ], );
}

my @data = Perlito5::AST::CompUnit::emit_perl5_program( [ gen_block() ] );

# print Perlito5::Dumper::ast_dumper( \@data );
my $out = [];
Perlito5::Perl5::PrettyPrinter::pretty_print( \@data, 0, $out );
print join( '', @$out ), "\n";

