# pX/Common/iterator_engine_p6rule.t - fglock
#
# experimental implementation of p6-rule parser
#
# see also: ../../Grammars/rx_grammar.pm

use strict;
use warnings;

# require 'iterator_engine.pl';
require 'iterator_engine_p6rule.pl';

use Test::More qw(no_plan);
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Pad = '# ';
my ( $program, $compiled );
my $match;
my $rule = \&grammar1::rule;

{
  $match = $rule->( '<word>' );
  #print Dumper( $match );
  ok ( $match->{bool}, "parse rule" );
  $program = emit_rule( $match->{capture} );
  # print "program:\n$program";
  ok ( defined $program, "emit rule to p5" );
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  $match = $compiled->( 'some_word' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample of text" );

  $match = $compiled->( '!some_word' );
  ok ( ! $match->{bool}, "rejects unmatching text" );
}

{
  my $rule = ::compile_rule( '<?word>', 
      {print_program=>0, print_ast=>0}  );
  is ( ref $rule, "CODE", "compile_rule( '<?word>' )" );
  $match = $rule->( 'aword%' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 1" );
  is ( $match->{tail}, '%', "correct left-out" );
}

{
  my $rule = ::compile_rule( '<!word>', 
      {print_program=>0, print_ast=>0}  );
  is ( ref $rule, "CODE", "compile_rule( '<!word>' )" );
  $match = $rule->( 'aword%' );
  # print Dumper( $match );
  ok ( ! $match->{bool}, "parse sample 1" );
  is ( $match->{tail}, undef, "correct left-out (fail)" );

  $match = $rule->( '%%%' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 1" );
  is ( $match->{tail}, '%%%', "correct left-out" );
}

{
  $match = $rule->( "<'lit'>" );
  #print Dumper( $match->{capture} );
  ok ( $match->{bool}, "parse rule <'lit'>" );
  $program = emit_rule( $match->{capture} );
  #print "program:\n$program";
  ok ( defined $program, "emit rule to p5" );
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  $match = $compiled->( 'lit' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample of text" );

  $match = $compiled->( '!some_word' );
  ok ( ! $match->{bool}, "rejects unmatching text" );
}

{
  $match = $rule->( '..' );
  ok ( $match->{bool}, "parse rule - dot-dot" );
  #print Dumper( $match );
  $program = emit_rule( $match->{capture} );
  #print "program:\n$program";
  ok ( defined $program, "emit rule to p5" );
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  $match = $compiled->( 'some_word' );
  #print Dumper( $match );
  ok ( $match->{bool}, "parse sample of text" );

  $match = $compiled->( '!' );
  ok ( ! $match->{bool}, "rejects unmatching text" );
}

{
  $match = $rule->( '(.)' );
  ok ( $match->{bool}, "parse rule - capture dot" );
  #print Dumper( $match->{capture} );
  $program = emit_rule( $match->{capture} );
  #print "program:\n$program";
  ok ( defined $program, "emit rule to p5" );
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  $match = $compiled->( 'some_word' );
  #print Dumper( $match );
  ok ( $match->{bool}, "parse sample of text" );
}

{
  $match = $rule->( '\(.\)' );
  ok ( $match->{bool}, "parse rule - escaped paren" );
  #print Dumper( $match->{capture} );
  $program = emit_rule( $match->{capture} );
  #print "program:\n$program";
  ok ( defined $program, "emit rule to p5" );
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  $match = $compiled->( '(s)' );
  #print Dumper( $match );
  ok ( $match->{bool}, "parse sample of text" );
}

{

  # XXX - allow whitespace everywhere
  # $match = $rule->( '<word>| [ <ws> <word> ]' );

  $match = $rule->( '<word>|[<ws><word>]' );
  ok ( $match->{bool}, "parse rule - alternates with grouping" );
  ok ( ! $match->{tail}, "full match" );
  #print "# match:\n", Dumper( $match );
  $program = emit_rule( $match->{capture} );
  ok ( defined $program, "emit rule to p5" );
  #print "# program: $program";
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  $match = $compiled->( 'some_word' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 1" );
  $match = $compiled->( ' other_word' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 2" );
  $match = $compiled->( '!' );
  ok ( !$match->{bool}, "rejects unmatching text" );
}

{
  $match = $rule->( '<word> <ws>' );
  ok ( $match->{bool}, "parse rule - 2 terms, with whitespace" );
  $program = emit_rule( $match->{capture} );
  ok ( defined $program, "emit rule to p5" );
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  $match = $compiled->( 'some_word other' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample of text" );
  is ( $match->{tail}, 'other', "remaining unmatched text (tail)" );

  $match = $compiled->( 'one_word' );
  ok ( !$match->{bool}, "rejects unmatching text" );
}

{
  $match = $rule->( '<word> <ws>*', undef, {capture=>1} );
  ok ( $match->{bool}, "parse rule - 2 terms, with star" );
  $program = emit_rule( $match->{capture} );
  ok ( defined $program, "emit rule to p5" );
  #print $program;
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  $match = $compiled->( 'some_word other' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample of text" );
  is ( $match->{tail}, 'other', "remaining unmatched text (tail)" );

  # this test doesn't apply
  #$match = $compiled->( 'one_word!' );
  #ok ( !$match->{bool}, "rejects unmatching text" );
}

{
  $match = $rule->( 'a+?', undef, {capture=>1} );
  ok ( $match->{bool}, "parse rule - a+?" );
  $program = emit_rule( $match->{capture} );
  #print "program:\n$program";
  ok ( defined $program, "emit rule to p5" );
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  $match = $compiled->( 'aaaasome_word' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample of text" );
  is ( $match->{tail}, 'aaasome_word', "correct left-out" );
}

{
  $match = $rule->( q(\' .*? \'), undef, {capture=>1} );
  ok ( $match->{bool}, "parse rule - \' .*? \'" );
  $program = emit_rule( $match->{capture} );
  #print "program:\n$program";
  ok ( defined $program, "emit rule to p5" );
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  $match = $compiled->( q('aaaa' 'some_word' ) );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample of text" );
  is ( $match->{tail}, q( 'some_word' ), "correct left-out" );
}

{
  $match = $rule->( q(\' .* \'), undef, {capture=>1} );
  ok ( $match->{bool}, "parse rule - \' .* \'" );
  $program = emit_rule( $match->{capture} );
  #print "program:\n$program";
  ok ( defined $program, "emit rule to p5" );
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  $match = $compiled->( q('aaaa' 'some_word' x) );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample of text" );
  is ( $match->{tail}, q( x), "correct left-out" );
}

{
  $match = $rule->( 'a?', undef, {capture=>1} );
  ok ( $match->{bool}, "parse rule - a?" );
  $program = emit_rule( $match->{capture} );
  #print "program:\n$program";
  ok ( defined $program, "emit rule to p5" );
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  $match = $compiled->( 'aaa' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 1" );
  is ( $match->{tail}, 'aa', "correct left-out" );
  $match = $compiled->( 'bbb' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 2" );
  is ( $match->{tail}, 'bbb', "correct left-out" );
}

{
  my $rule = ::compile_rule( 'a??', {print_program=>0, print_ast=>0} );
  is ( ref $rule, "CODE", "compile_rule( 'a??' )" );
  $match = $rule->( 'aaa' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 1" );
  is ( $match->{tail}, 'aaa', "correct left-out" );
}

{
  my $rule = ::compile_rule( 'a??b', {print_program=>0, print_ast=>0} );
  is ( ref $rule, "CODE", "compile_rule( 'a??b' )" );
  $match = $rule->( 'abaa' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 1" );
  is ( $match->{tail}, 'aa', "correct left-out" );
}

{
  $match = $rule->( '(<word>) <ws>' );
  ok ( $match->{bool}, "parse rule - 2 terms, with capture" );
  $program = emit_rule( $match->{capture} );
  #print $program;
  ok ( defined $program, "emit rule to p5" );
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  $match = $compiled->( 'some_word other' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample of text" );

  # TODO - test captured text

  $match = $compiled->( 'one_word' );
  ok ( !$match->{bool}, "rejects unmatching text" );
}

{
  $match = $rule->( '<word> [ <ws> <word> ]' );
  ok ( $match->{bool}, "parse rule - non-capturing group" );
  $program = emit_rule( $match->{capture} );
  #print $program;
  ok ( defined $program, "emit rule to p5" );
  #print $program;
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  $match = $compiled->( 'some_word other' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample of text" );

  $match = $compiled->( '!some_word' );
  ok ( !$match->{bool}, "rejects unmatching text" );
}

{
  $match = $rule->( '<word>|<ws>' );
  ok ( $match->{bool}, "parse rule - alternates" );
  #print "# match:\n", Dumper( $match );
  $program = emit_rule( $match->{capture} );
  ok ( defined $program, "emit rule to p5" );
  # print "# program: $program";
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  $match = $compiled->( 'some_word' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 1" );
  $match = $compiled->( ' ' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 2" );
  $match = $compiled->( '!' );
  ok ( !$match->{bool}, "rejects unmatching text" );
}

{
  $match = $rule->( '<word>|<ws>|.' );
  ok ( $match->{bool}, "parse rule - 3 alternates" );
  ok ( ! $match->{tail}, "full match" );
  #print "# match:\n", Dumper( $match );
  $program = emit_rule( $match->{capture} );
  ok ( defined $program, "emit rule to p5" );
  #print "# program: $program";
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  $match = $compiled->( 'some_word' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 1" );
  $match = $compiled->( ' ' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 2" );
  $match = $compiled->( '-' );
  ok ( $match->{bool}, "parse sample 3" );
}

{
  my $rule = ::compile_rule( 'xxx', {print_program=>0} );
  is ( ref $rule, "CODE", "compile_rule( 'xxx' )" );
  $match = $rule->( 'xxxaaa' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 1" );
  is ( $match->{tail}, 'aaa', "correct left-out" );
  $match = $rule->( 'bbb' );
  # print Dumper( $match );
  ok ( ! $match->{bool}, "reject sample 2" );
}

{
  $match = grammar1::variable( '$test::xxx--' );
  # print Dumper( $match );
  ok ( $match->{bool}, "match variable" );
  is ( $match->{tail}, '--', "correct left-out" );
}

{
  local $test::xxx = '123';
  my $rule = ::compile_rule( '$test::xxx', {print_program=>0, print_ast=>0} );
  is ( ref $rule, "CODE", "compile_rule( '\$test::xxx' )" );
  $match = $rule->( '123aaa' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 1" );
  is ( $match->{tail}, 'aaa', "correct left-out" );
}

{
  local @test::xxx = ( 
      ::compile_rule( '123' ), 
      ::compile_rule( 'abc' )
  );
  my $rule = ::compile_rule( '<@test::xxx>', {print_program=>0, print_ast=>0} );
  is ( ref $rule, "CODE", "compile_rule( '<{\@test::xxx}>' ) array of rule" );

  $match = $rule->( '123aaa' );

  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 1" );
  is ( $match->{tail}, 'aaa', "correct left-out" );

  $match = $rule->( 'abcaaa' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 2" );
  is ( $match->{tail}, 'aaa', "correct left-out" );

  push @test::xxx, ::compile_rule( 'cde' );
  $match = $rule->( 'cdeaaa' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 3 after run-time modification" );
  is ( $match->{tail}, 'aaa', "correct left-out" );

}

{
  my $rule = ::compile_rule( '$xyz := (abc)', {print_program=>0, print_ast=>0} );
  is ( ref $rule, "CODE", "named capture" );
  $match = $rule->( 'abcaaa' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 1" );
  is ( $match->{tail}, 'aaa', "correct left-out" );
}

{
  $test::s = 1;
  my $rule = ::compile_rule( '(abc) { $test::s = 2 }', {print_program=>0} );
  is ( ref $rule, "CODE", "closure" );
  $match = $rule->( 'abcaaa' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 1" );
  is ( $match->{tail}, 'aaa', "correct left-out" );
  is ( $test::s, 2, "closure was run" );
}

{
  my $rule = ::compile_rule( '(abc) { return {abc => "xyz",} }', {print_program=>0} );
  is ( ref $rule, "CODE", "capture closure with pair" );
  $match = $rule->( 'abcaaa' );
  is ( @{ $match->{capture} }, 1, 'only one item was captured' );
  #print Dumper( $match->{capture} );
}

{
  my $rule = ::compile_rule( '(abc) { return {abc => $<>,} }', {print_program=>0} );
  is ( ref $rule, "CODE", "capture closure with pair + capture" );
  $match = $rule->( 'abcaaa' );
  is ( @{ $match->{capture} }, 1, 'only one item was captured' );
  # print Dumper( $match->{capture} );
}

{
  my $rule = ::compile_rule( '(abc) { return "xyz" }', {print_program=>0} );
  is ( ref $rule, "CODE", "capture closure" );
  $match = $rule->( 'abcaaa' );
  is ( @{ $match->{capture} }, 1, 'only one item was captured' );
  #print Dumper( $match->{capture} );
  ok ( $match->{bool}, "parse sample 1" );
  is ( $match->{tail}, 'aaa', "correct left-out" );

  my $rule2 = ::compile_rule( '(def) { return "123" }', {print_program=>0} );

  {
    my $rule3 = ruleop::alternation( [ 
        ruleop::try( $rule ), 
        ruleop::try( $rule2 ),
      ] 
    );
    $match = $rule3->( 'abcaaa' );
    is ( @{ $match->{capture} }, 1, 'capture closure + alternation 1' );
    # print Dumper( $match->{capture} );
    ok ( $match->{bool}, "parse sample 1" );
    is ( $match->{tail}, 'aaa', "correct left-out" );
    
    $match = $rule3->( 'defaaa' );
    is ( @{ $match->{capture} }, 1, 'capture closure + alternation 2' );
    # print Dumper( $match->{capture} );
    ok ( $match->{bool}, "parse sample 2" );
    is ( $match->{tail}, 'aaa', "correct left-out" );
  }
  
  {
    my $rule4 = ruleop::concat( 
      ruleop::try( $rule ), 
      ruleop::try( $rule2 ),
    );
    $match = $rule4->( 'abcdefaaa' );
    is ( @{ $match->{capture} }, 2, 'capture closure + concat, 2 items were captured' );
    # print Dumper( $match->{capture} );
    ok ( $match->{bool}, "parse sample 1" );
    is ( $match->{tail}, 'aaa', "correct left-out" );
  }
}

__END__

{
  local @test::xxx = ( '123', 'abc' );
  my $rule = ::compile_rule( '<@test::xxx>', {print_program=>1} );
  is ( ref $rule, "CODE", "compile_rule( '<{\@test::xxx}>' array of str)" );

  $match = $rule->( '123aaa' );

  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 1" );
  is ( $match->{tail}, 'aaa', "correct left-out" );

  $match = $rule->( 'abcaaa' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 2" );
  is ( $match->{tail}, 'aaa', "correct left-out" );

  push @test::xxx, 'cde';
  $match = $rule->( 'cdeaaa' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample 3 after run-time modification" );
  is ( $match->{tail}, 'aaa', "correct left-out" );

}

__END__

# TODO - test backtracking (implement '*' first)

# TODO - convert older tests to Test::More

print Dumper ruleop::rule()->( 0, '{ print 1+1, "\n" }' )
                     ->( 0, '' );

print Dumper ruleop::rule( 0, ' ' )
                     ->( 0, 'x' );

print Dumper ruleop::rule( 0, '<word>' )
                     ->( 0, ' abc def' );

print Dumper ruleop::rule( 0, 'abc' )
                     ->( 0, 'abc' );
print Dumper ruleop::rule( 0, '<word>' )
                     ->( 0, 'abc' );

__END__

=for later

$state = 0;
{
    while ( defined $state ) {
        ($state, $tmp, undef) = 
            ruleop::greedy_plus( rule::constant( 'ab' ) )
            ->( $state, qw(a b a b a b) );
        print "recursive\n", Dumper( $tmp );
    }
}

__END__

=for tested

$state = 0;
{
    my $alt = rule::alternation( 
        rule::constant('ab'), rule::constant('ba'), rule::constant('a') );
    while ( defined $state ) {
        ($state, $tmp, undef) = rule::concat( 
                $alt, rule::optional( $alt ) 
            )->( $state, qw(a b a)
        );
        print "concat\n", Dumper( $tmp );
    }
}

$state = 0;
while ( defined $state ) {
    print "alternation\n", Dumper( 
        ($state, $tmp, undef) = rule::alternation( 
            rule::constant('ab'), rule::constant('a') 
        )->( $state, qw(a b c) ) 
    );
}

$state = 0;
while ( defined $state ) {
    print "greedy\n", Dumper( 
        ($state, $tmp, undef) = rule::greedy( rule::constant('a') )->( $state, qw(a a b c) ) 
    );
}

$state = 0;
while ( defined $state ) {
    print "greedy + alternation state\n", Dumper( 
        ($state, $tmp, undef) = rule::greedy( 
            rule::alternation( 
                rule::constant('ab'), rule::constant('a'), 
                rule::constant('cd'), rule::constant('ba'), 
                rule::constant('bc'), )
        )->( $state, qw(a b a b c) ) 
    );
}

=cut

__END__

$state = 0;
for ( 0 .. 3 ) {
    print "greedy + alternation state $_\n", Dumper( 
        ($state, undef, undef) = rule::greedy( 
            rule::alternation( 
                rule::constant('cd'), rule::constant('ab'), 
                rule::constant('a'),  rule::constant('cd') )
        )->( $state, qw(a b a b c) ) 
    );
}

__END__

print "any-char\n", Dumper( 
  &{'rule::.'}( 0, qw( a b ) ) 
);

print "greedy\n", Dumper( 
  rule::greedy( rule::constant('a') )->( 0, qw(a a a b c) ) 
);
print "greedy backtrack\n", Dumper( 
  rule::concat( 
    rule::greedy( rule::constant('a') ),
    \&{'rule::.'} 
  )->( 0, qw(a a a a) ) 
);
print "greedy no-match\n", Dumper( 
  rule::concat(
    rule::greedy( rule::constant('a') ),
    \&{'rule::.'}
  )->( 0, qw(b a a a a) ) 
);
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
