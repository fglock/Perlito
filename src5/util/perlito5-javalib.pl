package Perlito5;
use strict;
use warnings;

use Perlito5::Compiler;
use Perlito5::Java::Emitter;
use Perlito5::Java::Runtime;
use Perlito5::Java::Lib;

$Perlito5::JAVA_EVAL = 1;

# sub compile_p5_to_java {
#     my $s = shift;
#     $Perlito5::PKG_NAME = 'main';
#     $Perlito5::PROTO    = {};
#     my $ast = Perlito5::Grammar::exp_stmts($s, 0);
#     Perlito5::AST::CompUnit::emit_java_program(
#         [
#             Perlito5::AST::CompUnit->new( name => 'main', body => Perlito5::Match::flat($ast) )
#         ]
#     );
# }

1;

