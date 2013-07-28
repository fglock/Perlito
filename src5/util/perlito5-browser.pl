package Perlito5;

use strict;
use warnings;

use Perlito5::Match;
use Perlito5::Grammar;
use Perlito5::Grammar::Control;
use Perlito5::Precedence;
use Perlito5::Expression;
use Perlito5::Macro;
use Perlito5::Runtime;

use Perlito5::Javascript2::Emitter;
use Perlito5::Javascript2::Runtime;
# use Perlito5::Javascript2::Array;
# use Perlito5::Javascript2::CORE;
# use Perlito5::Javascript2::IO;
# use Perlito5::Javascript2::Sprintf;

sub compile_p5_to_js {
    my $s = shift;
    $Perlito5::PKG_NAME = 'main';
    $Perlito5::PROTO    = {};
    my $ast = Perlito5::Grammar->exp_stmts($s, 0);
    Perlito5::AST::CompUnit::emit_javascript2_program(
        [
            Perlito5::AST::CompUnit->new( name => 'main', body => Perlito5::Match::flat($ast) )
        ]
    );
}

1;

