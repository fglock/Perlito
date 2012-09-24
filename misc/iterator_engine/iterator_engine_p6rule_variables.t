# pX/Common/iterator_engine_p6regex.pl - fglock
#
# experimental implementation of p6-regex parser
#
# see also: ../../Grammars/rx_grammar.pm

use strict;
use warnings;

# XXX - TODO - update $<> to $()

# require 'iterator_engine.pl';
require 'iterator_engine_p6rule.pl';

use Test::More qw(no_plan);
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Pad = '  ';
my ( $program, $compiled );
my $match;
my $rule = \&grammar1::rule;

{
  diag "plain match";
  my $rule = ::compile_rule( 'abc', {print_program=>0} );
  my $matched = { 
    'constant' => 'abc' 
  };
  my $captured = undef;
  $match = $rule->( 'abcaaa' );
  # print "match: ", Dumper( $match );
  is_deeply ( match::get( $match, '$<>' ), $captured, 'variable: $<>' );
  is_deeply ( match::get( $match, '$/<>' ), $captured, 'variable: $/<>' );
  is_deeply ( match::get( $match, '$/' ), $matched, 'variable: $/' );
}

{
  diag "simple capture";
  my $rule = ::compile_rule( '123(abc)456', {print_program=>0} );
  # -- '$matched' is not tested because it is too complex
  # my $matched = ...
  my $captured = [
      {
        'capturing_group' => [ 'abc' ],
      }
  ];
  $match = $rule->( '123abc456' );
  # print "match: ", Dumper( $match );
  is_deeply ( match::get( $match, '$<>' ), $captured, 'variable: $<>' );
  is_deeply ( match::get( $match, '$/<>' ), $captured, 'variable: $/<>' );
  # is_deeply ( match::get( $match, '$/' ), $matched, 'variable: $/' );
}

{
  diag "deep capture";
  my $rule = ::compile_rule( '123(ab(cd)ef)456', {print_program=>0} );
  my $captured = [
      {
        'capturing_group' => [
          'ab',
          {
            'capturing_group' => [
              'cd'
            ]
          },
          'ef'
        ]
      }
    ];
  $match = $rule->( '123abcdef456' );
  # print "match: ", Dumper( $match );
  is_deeply ( match::get( $match, '$<>' ), $captured, 'variable: $<>' );
  is_deeply ( match::get( $match, '$/<>' ), $captured, 'variable: $/<>' );
}

{
  diag "capture with {return} block";
  $test::test = { 'got' => 'xyz' };
  my $rule = ::compile_rule( '(abc) { return $test::test }', {print_program=>0} );
  my $captured = [ $test::test ];
  $match = $rule->( 'abcaaa' );
  is_deeply ( match::get( $match, '$<>' ),  $captured, 'variable: $<>' );
  is_deeply ( match::get( $match, '$/<>' ), $captured, 'variable: $/<>' );
  # XXX - not sure about this, because as the match was aborted by return,
  #       it may have an invalid value
  #print Dumper( $match );
  #print Dumper( match::get( $match, '$/' ) );
  #is_deeply ( match::get( $match, '$/' ), undef, 'variable: $/' );
}

{
  diag "numbered capture";
  my $rule = ::compile_rule( '123(abc)456(def)789', {print_program=>0} );
  $match = $rule->( '123abc456def789' );
  # print "match: ", Dumper( $match );
  is_deeply ( match::get( $match, '$<0>' ), ['abc'], 'variable: $<0>' );
  is_deeply ( match::get( $match, '$<1>' ), ['def'], 'variable: $<1>' );
  is ( match::get( $match, '$<2>' ), undef, 'no variable: $<2>' );
}

{
  diag "named capture with {return} block";
  $test::test = { 'got' => ['xyz'] };
  my $rule = ::compile_rule( '(abc) { return $test::test }', {print_program=>0} );
  $match = $rule->( 'abcaaa' );
  is_deeply ( match::get( $match, '$<got>' ), ['xyz'], 'variable: $<return_block>' );
}

{
  diag "named capture with <subrule>";
  *test::test = ::compile_rule( 'abc' );
  my $rule = ::compile_rule( '<test::test>', {print_program=>0} );
  $match = $rule->( 'abcaaa' );
  is_deeply ( match::get( $match, '$<test::test>' ), ['abc'], 'variable: <subrule>' );
}

__END__
