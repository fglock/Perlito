use v6-alpha;
use MiniPerl6::Grammar;
use MiniPerl6::Perl5::Emitter;

{
  my $p = MiniPerl6::Grammar.exp( '1 + 1' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( '1 + 2 + 3' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( '$a := 1 + 2 + 3' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( '1 ?? 2 !! 3' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( '$a := 1 + substr( $a, 2 )' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( '$a.bool( 10 )' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( '$a.bool( 10 + (10) )' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( '$m.to' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( '((( ( substr( $str, $m.to, 1) eq \'a\'
  ) )))' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( '1 + $m.to' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( '(1 + $m.to( 1 + $m.to ))' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( '
    ( 123 ) ?? 42 !! 43  
   '
  );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( '
    ( \'a\'
    eq substr( $str, $m.to, 1) )
    ?? 42 !! 43  
   '
  );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( '
    $m.bool( ((( \'a\'
    eq substr( $str, $m.to, 1)   ?? (1 + $m.to( 1 + $m.to ))  !! (0) ) && ( \'b\'
    eq substr( $str, $m.to, 1)   ?? (1 + $m.to( 1 + $m.to ))  !! (0) ) && ( \'c\'
    eq substr( $str, $m.to, 1)   ?? (1 + $m.to( 1 + $m.to ))  !! (0) ))))
  '
  );
  say ($$p).perl;
  say ($$p).emit;
}
