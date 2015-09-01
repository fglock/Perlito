#
# $ perl -Isrc5/lib misc/generate.pl 
#

use Perlito5::Perl5::Emitter;
use Perlito5::Perl5::PrettyPrinter;

sub r { int( sqrt( rand($_[0] // 5) ) ) }

my @v = qw/ a b x self /;
my @bool = qw{ infix:<&&> infix:<&&> infix:<||> infix:<||> infix:<//> };
my @oper = qw{ infix:<+> infix:<-> infix:<*> infix:</> };
my @compare = qw{ infix:<==> infix:<>=> infix:<<=> infix:<>> infix:<<> };
my @hash_op = qw{ keys values };

sub gen_ident {
    my (%args) = @_;
    return $v[ rand(@v) ];
}

sub gen_var {
    my (%args) = @_;
    my $r = rand();
    if ( $r > 0.96 ) {
        return Perlito5::AST::Index->new(
            index_exp => gen_exp(%args),
            obj => gen_var(%args),
        );
    }
    if ( $r > 0.92 ) {
        return Perlito5::AST::Lookup->new(
            index_exp => gen_exp(%args),
            obj => gen_var(%args),
        );
    }
    return Perlito5::AST::Var->new( sigil => '$', name => gen_ident(%args), _decl => 'my' );
}

sub gen_bool {
    my (%args) = @_;
    $args{level}++;
    my $r = rand();
    return gen_exp(%args)
        if $args{level} > 7;
    if ( $r > 0.5 ) {
        return Perlito5::AST::Apply->new(
            code => $bool[ rand(@bool) ],
            arguments => [ gen_compare(%args), gen_compare(%args) ],
        );
    }
    return gen_var(%args);
}

sub gen_compare {
    my (%args) = @_;
    $args{level}++;
    my $r = rand();
    return gen_exp(%args)
        if $args{level} > 7;
    if ( $r > 0.5 ) {
        return Perlito5::AST::Apply->new(
            code => $compare[ rand(@compare) ],
            arguments => [ gen_exp(%args), gen_exp(%args) ],
        );
    }
    return gen_exp(%args);
}

sub gen_exp {
    my (%args) = @_;
    $args{level}++;
    my $r = rand();
    return gen_var(%args)
        if $args{level} > 7;
    if ( $r > 0.80 ) {
        return Perlito5::AST::Apply->new(
            code => $oper[ rand(@oper) ],
            arguments => [ gen_exp(%args), gen_exp(%args) ],
        );
    }
    if ( $r > 0.75 ) {
        return Perlito5::AST::Apply->new(
            code => "infix:<=>",
            arguments => [ gen_var(%args), gen_exp(%args) ],
        );
    }
    if ( $r > 0.70 ) {
        return Perlito5::AST::Apply->new(
            code => $hash_op[ rand(@hash_op) ],
            arguments => [ Perlito5::AST::Apply->new(
                            code => 'prefix:<%>', arguments => [ gen_var(%args) ] ),
                         ],
        );
    }
    return gen_var(%args);
}

sub gen_stmt {
    my (%args) = @_;
    $args{level}++;
    my $r = rand();
    return gen_exp(%args)
        if $args{level} > 7;
    if ( $r > 0.7 ) {
        return Perlito5::AST::If->new(
            cond => gen_bool(%args),
            body => gen_block(%args),
            (rand() > 0.5 ? ( otherwise => gen_block(%args) ) : () ),
        );
    }
    if ( $r > 0.6 ) {
        return Perlito5::AST::While->new(
            cond => gen_bool(%args),
            body => gen_block(%args),
        );
    }
    if ( $r > 0.3 ) {
        return Perlito5::AST::Apply->new(
            code => "infix:<=>",
            arguments => [ gen_var(%args), gen_exp(%args) ],
        );
    }
    return gen_exp;
}

sub gen_block {
    my (%args) = @_;
    my $size;
    if ($args{level} == 1) {
        $size = r(30);
    }
    else {
        $size = r();
    }
    $args{level}++;
    return Perlito5::AST::Block->new( stmts => [ map { gen_stmt(%args) } 0 .. $size ], );
}

my @data = Perlito5::AST::CompUnit::emit_perl5_program( [ gen_block(level => 1) ] );

# print Perlito5::Dumper::ast_dumper( \@data );
my $out = [];
Perlito5::Perl5::PrettyPrinter::pretty_print( \@data, 0, $out );
print join( '', @$out ), "\n";

