use v6-alpha;

use MiniPerl6::Grammar;

{
  my $p = MiniPerl6::Grammar.var( '$abc' );
  ($$p).perl.say;
}

{
  my $p = MiniPerl6::Grammar.exp( '$abc' );
  ($$p).perl.say;
}

{
  my $p = MiniPerl6::Grammar.exp_seq( '$abc' );
  ($$p).perl.say;
}

{
  my $p = MiniPerl6::Grammar.exp_seq( '$abc, $def, $xyz' );
  ($$p).perl.say;
}

{
  my $p = MiniPerl6::Grammar.exp_seq( '$abc, $def, $xyz,' );
  ($$p).perl.say;
}

{
  my $p = MiniPerl6::Grammar.exp_seq( '$xyz,' );
  ($$p).perl.say;
}

{
  my $p = MiniPerl6::Grammar.apply( 'mysub( $abc, $def, $xyz )' );
  ($$p).perl.say;
}

{
  my $p = MiniPerl6::Grammar.exp( '$obj.meth( $abc, $def, $xyz )' );
  ($$p).perl.say;
}

{
  my $p = MiniPerl6::Grammar.exp( '$obj := $xyz' );
  ($$p).perl.say;
}

{
  my $p = MiniPerl6::Grammar.exp_mapping( '$obj => $xyz' );
  ($$p).perl.say;
}

{
  my $p = MiniPerl6::Grammar.exp_mapping( '$obj => $xyz, $a => $b' );
  ($$p).perl.say;
}

{
  my $p = MiniPerl6::Grammar.lit_object( '::Tree($a => $x, $b => $y)' );
  ($$p).perl.say;
}

{
  my $p = MiniPerl6::Grammar.ctrl_return( 'return $a' );
  ($$p).perl.say;
}

{
  my $p = MiniPerl6::Grammar.while( 'while $a { $b }' );
  ($$p).perl.say;
}

{
  my $p = MiniPerl6::Grammar.for( 'for $a -> $b { $b }' );
  ($$p).perl.say;
}

{
  my $p = MiniPerl6::Grammar.when( 'when $b { $c }' );
  ($$p).perl.say;
}

{
  my $p = MiniPerl6::Grammar.if( 'if $b { $c } else { $d }' );
  ($$p).perl.say;
}

{
  my $p = MiniPerl6::Grammar.val( 'undef' );
  ($$p).perl.say;
}

{
  my $p = MiniPerl6::Grammar.val( '10' );
  ($$p).perl.say;
}

{
  my $p = MiniPerl6::Grammar.exp( ' $abc := True ' );
  ($$p).perl.say;
}

{
  my $p = MiniPerl6::Grammar.val( '"mo\"ose"' );
  ($$p).perl.say;
}

{
  my $p = MiniPerl6::Grammar.comp_unit( 'class Moose { say(123, 456); 123 := 410; 123.moose(1) }' );
  ($$p).perl.say;
}

