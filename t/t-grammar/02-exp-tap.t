use v6-alpha;
use MiniPerl6::Grammar;
use MiniPerl6::Perl5::Emitter;

say "1..15";

sub test( $mp6, $p5 ) {
  my $p = MiniPerl6::Grammar.exp( $mp6 );
  #say ($$p).perl;
  my $t = ($$p).emit;
  print 'not ' if $t ne $p5;
  say 'ok - ', $mp6; 
  say '# got: ', $t if $t ne $p5;
  say '# expected: ', $p5 if $t ne $p5;
}

test( '1+1', '1 + 1' );
test( '1+2+3', '1 + 2 + 3' );
test( '$a := 1 + 2 + 3', '$a = 1 + 2 + 3' );
test( '1 ?? 2 !! 3', '(1 ? 2 : 3)' );
test( '$a := 1 + substr( $a, 2 )', '$a = 1 + substr($a, 2)' );
test( '$a.bool( 10 )', '$a->bool(10)' );
test( '$a.bool( 10 + (10) )', '$a->bool(10 + 10)' );
test( '$m.to', '$m->to()' );
test( '((( ( substr( $str, $m.to, 1) eq \'a\'  ) )))',
      'substr($str, $m->to(), 1) eq \'a\'
' );
test( '1 + $m.to', '1 + $m->to()' );
test( '1 + $m.to(1 + $m.to)', '1 + $m->to(1 + $m->to())' );
test( '(1) ?? 2 !! 3', '(1 ? 2 : 3)' );
test( '( \'a\' eq substr( $str, $m.to, 1) ) ?? 42 !! 43 )',
      '(\'a\'
 eq substr($str, $m->to(), 1) ? 42 : 43)' );
test( '1 && 2 && 3', '1 && 2 && 3' );
test( '$m.bool(((( substr( $str, $m.to, 1) eq \'a\' ?? (1 + $m.to( 1 + $m.to )) !! (0) ))))',
      '$m->bool(substr($str, $m->to(), 1) eq (\'a\'
 ? 1 + $m->to(1 + $m->to()) : 0))' );
test( 'my $x', 'my $x' );

{
  my $p = MiniPerl6::Grammar.exp( '
    $m.bool(((( \'a\'
    eq substr( $str, $m.to, 1)   ?? (1 + $m.to( 1 + $m.to ))  !! (0) ) && ( \'b\'
    eq substr( $str, $m.to, 1)   ?? (1 + $m.to( 1 + $m.to ))  !! (0) ) && ( \'c\'
    eq substr( $str, $m.to, 1)   ?? (1 + $m.to( 1 + $m.to ))  !! (0) ))))
  '
  );
  # say ($$p).perl;
  say ($$p).emit;
}
