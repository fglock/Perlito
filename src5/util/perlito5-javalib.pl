package Perlito5;
use strict;
use warnings;

use Perlito5::Compiler;
use Perlito5::Java::Emitter;
use Perlito5::Java::Runtime;
use Perlito5::Java::Lib;

$Perlito5::JAVA_EVAL = 1;
$Perlito5::CODE_TOO_LARGE = 1;  # work around java code size limitation

Perlito5::Java::Lib::init();

1;

