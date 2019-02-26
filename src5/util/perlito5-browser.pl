package Perlito5;
use strict;
use warnings;

use Perlito5::Compiler;
use Perlito5::JavaScript2::Emitter;
use Perlito5::JavaScript2::Runtime;

sub compile_p5_to_js {
    my $s = shift;
    $Perlito5::PKG_NAME = 'main';
    Perlito5::init_proto();
    my $ast = Perlito5::Grammar::exp_stmts($s, 0);
    Perlito5::AST::CompUnit::emit_javascript2_program(
        [
            Perlito5::AST::CompUnit->new( name => 'main', body => Perlito5::Match::flat($ast) )
        ]
    );
}

1;

