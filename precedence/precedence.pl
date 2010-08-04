
class Main {
    use MiniPerl6::Expression;
   

    my $s = '; $a|$b| ($c+$y) & $x ;...';
    my $res = MiniPerl6::Expression.exp_parse( $s, 1 );
    say ($$res).perl;
    say "from: ", $res.from, " to: ", $res.to, " tail: ", substr($s, $res.to);

    my $s = '; $aaa ?? $xxx !! $yyy  ;...';
    my $res = MiniPerl6::Expression.exp_parse( $s, 1 );
    say ($$res).perl;
    say "from: ", $res.from, " to: ", $res.to, " tail: ", substr($s, $res.to);

    my $s = '; $a + b($c+$y).m($x) ;...';
    my $res = MiniPerl6::Expression.exp_parse( $s, 1 );
    say ($$res).perl;
    say "from: ", $res.from, " to: ", $res.to, " tail: ", substr($s, $res.to);

    my $s = '; a 1,2,3 and b ;...';
    my $res = MiniPerl6::Expression.exp_parse( $s, 1 );
    say ($$res).perl;
    say "from: ", $res.from, " to: ", $res.to, " tail: ", substr($s, $res.to);

    my $s = '; .a && .1 && .meth1 && $a.meth2 ;...';
    my $res = MiniPerl6::Expression.exp_parse( $s, 1 );
    say ($$res).perl;
    say "from: ", $res.from, " to: ", $res.to, " tail: ", substr($s, $res.to);

}
