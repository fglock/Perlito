use v6-alpha;
use MiniPerl6::Grammar;
use MiniPerl6::Perl5::Emitter;

{
  my $p = MiniPerl6::Grammar.exp( ' $abc := 123 ' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( ' $abc ' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( ' $abc := $x ' );
  say ($$p).perl;
  say ($$p).emit;
}

