package Perlito5;
use strict;
use warnings;

use Perlito5::Compiler;
use Perlito5::Perl6::Emitter;
use Perlito5::Perl6::PrettyPrinter;
use Perlito5::JavaScript2::Emitter;
use Perlito5::JavaScript2::Runtime;

sub compile_p5_to_p6 {
    my $s = shift;
    $Perlito5::PKG_NAME = 'main';
    Perlito5::init_proto();
    my $ast = Perlito5::Grammar::exp_stmts($s, 0);
    my @data = Perlito5::AST::CompUnit::emit_perl6_program(
        [
            Perlito5::AST::CompUnit->new( name => 'main', body => Perlito5::Match::flat($ast) )
        ]
    );
    my $out = [];
    Perlito5::Perl6::PrettyPrinter::pretty_print( \@data, 0, $out );
    return join( '', "use v6;\n", 
                     @$out, "\n",
               );

}

1;

