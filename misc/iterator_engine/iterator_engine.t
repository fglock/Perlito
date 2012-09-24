# pX/Common/iterator_engine_p6rule.t - fglock

use strict;
use warnings;

require 'iterator_engine.pl';

use Test::More qw(no_plan);
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Pad = '# ';
my ( $rule, $match );

{
  $rule = 
    ruleop::non_greedy_plus( 
      ruleop::alternation( [
        ruleop::constant( 'a' ), 
        ruleop::constant( 'c' ), 
      ] ),
    );
  $match = $rule->( 'a123', undef, {capture=>1} );
  ok ( $match->{bool}, "/[a|c]/ #1" );
  is ( $match->{tail}, '123', "tail is ok" );
  $match = $rule->( 'c123', undef, {capture=>1} );
  ok ( $match->{bool}, "/[a|c]/ #2" );
  is ( $match->{tail}, '123', "tail is ok" );
  #print Dumper( $match );
}

{
  $rule = 
    ruleop::greedy_star( 
      ruleop::constant( 'a' ) 
    );
  is ( ref $rule, "CODE", "rule 'a*' is a coderef" );
  $match = $rule->( 'aa' );
  # print Dumper( $match );
  ok ( $match->{bool}, "/a*/" );
  #print Dumper( $match );
  $match = $rule->( '' );
  ok ( $match->{bool}, "matches 0 occurrences" );
  #print Dumper( $match );
}

{
  $rule = 
    ruleop::greedy_plus( 
      ruleop::constant( 'a' ) 
    );
  $match = $rule->( 'aa' );
  ok ( $match->{bool}, "/a+/" );
  $match = $rule->( '!!' );
  ok ( ! $match->{bool}, "rejects unmatching text" );
}

{
  $rule = 
    ruleop::concat(
      ruleop::greedy_plus( 
        ruleop::alternation( [
          ruleop::constant( 'a' ), 
          ruleop::constant( 'c' ), 
        ] ),
      ),
      ruleop::constant( 'ab' )
    );
  $match = $rule->( 'aacaab' );
  ok ( $match->{bool}, "/[a|c]+ab/ with backtracking" );
  # print Dumper( $match );
}

print "# XXX other tests disabled due to a big API change\n";
__END__

{
  $rule = 
    ruleop::non_greedy_plus( 
      ruleop::alternation( [
        ruleop::constant( 'a' ), 
        ruleop::constant( 'c' ), 
      ] ),
    );
  ( $stat, $assertion, $match, $tail ) = $rule->( 'aacaab', undef, {capture=>1} );
  ok ( defined $match, "/[a|c]+/" );
  is ( $tail, 'acaab', "tail is ok" );
  #print Dumper( $match );
}

{
  $rule = 
    ruleop::concat(
      ruleop::non_greedy_plus( 
        ruleop::alternation( [
          ruleop::constant( 'a' ), 
          ruleop::constant( 'c' ), 
        ] ),
      ),
      ruleop::constant( 'cb' )
    );
  ( $stat, $assertion, $match, $tail ) = $rule->( 'aacacb' );
  ok ( defined $match, "/[a|c]+?ab/ with backtracking" );
  #print Dumper( $match );
}

{
  # tests for a problem found in the '|' implementation in p6rule parser
  
  my $rule = 
    ruleop::constant( 'a' );
  my $alt = 
    ruleop::concat(
        $rule,
        ruleop::optional (
            ruleop::concat(
                ruleop::constant( '|' ),
                $rule
            )
        )
    );
  ( $stat, $assertion, $match, $tail ) = $alt->( 'a' );
  ok ( defined $match, "/a|a/ #1" );
  ( $stat, $assertion, $match, $tail ) = $alt->( 'a|a' );
  ok ( defined $match, "/a|a/ #2" );

  # adding '*' caused a deep recursion error (fixed)

  $alt = 
    ruleop::concat(
        $rule,
        ruleop::greedy_star(
          ruleop::concat(
              ruleop::constant( '|' ),
              $rule
          )
        )
    );
  ( $stat, $assertion, $match, $tail ) = $alt->( 'a' );
  ok ( defined $match, "/a [ |a ]*/ #1" );
  ( $stat, $assertion, $match, $tail ) = $alt->( 'a|a' );
  ok ( defined $match, "/a [ |a ]*/ #2" );
  ( $stat, $assertion, $match, $tail ) = $alt->( 'a|a|a' );
  ok ( defined $match, "/a [ |a ]*/ #3" );

}

__END__

# old tests 

print "word\n", Dumper( 
  &{'rule::<word>'}( 0, qw(b a a ! !) ) 
);
print "word concat\n", Dumper( 
  rule::concat( \&{'rule::<word>'}, \&{'rule::<ws>'} )->( 0, qw(b a ),' ' ) 
);
print "non_greedy + backtracking\n", Dumper( 
  rule::concat(
    rule::non_greedy( rule::constant('a') ),
    rule::constant('ab')
  )->( 0, qw(a a a a b) ) 
);
print "alternation + backtracking\n", Dumper( 
  rule::concat(
    rule::alternation( rule::constant('a'), rule::constant('ab') ),
    rule::constant('ab')
  )->( 0, qw(a b a b) ) 
);
print "alternation + greedy + backtracking -- (ab,a,ab)(ab)\n", Dumper( 
  rule::concat(
    rule::greedy(
      rule::alternation( rule::constant('a'), rule::constant('ab') )
    ),
    rule::constant('ab')
  )->( 0, qw(a b a a b a b) ) 
);
