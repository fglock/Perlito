package Perlito5;

use Perlito5::Match;
use Perlito5::Javascript::Emitter;
use Perlito5::Grammar;
use Perlito5::Grammar::Control;
use Perlito5::Precedence;
use Perlito5::Expression;
use Perlito5::Macro;
use Perlito5::Runtime;

sub compile_p5_to_js {
    my $s = shift;
    $Perlito5::PKG_NAME = 'main';
    $Perlito5::PROTO    = {};
    my $ast = Perlito5::Grammar->exp_stmts($s, 0);
    Perlito5::AST::CompUnit::emit_javascript_program(
        [
            Perlito5::AST::CompUnit->new( name => 'main', body => $ast->flat() )
        ]
    );
}

1;

