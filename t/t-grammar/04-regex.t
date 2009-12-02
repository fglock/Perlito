use v6-alpha;
use MiniPerl6::Grammar;
use MiniPerl6::Perl5::Emitter;
use MiniPerl6::Grammar::Regex;
use MiniPerl6::Emitter::Token;

{
  my $p = MiniPerl6::Grammar.method( 'method meth ( $x ) { my $y }' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( 'token { abc }' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( 'token mytok { abc }' );
  say ($$p).perl;
  say ($$p).emit;
}

