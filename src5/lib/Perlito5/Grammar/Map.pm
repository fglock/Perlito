
package Perlito5::Grammar::Map;

use strict;

Perlito5::Precedence::add_term( 'map'   => sub { Perlito5::Grammar::Map->term_map_or_sort( $_[0], $_[1] ) } );
Perlito5::Precedence::add_term( 'sort'  => sub { Perlito5::Grammar::Map->term_map_or_sort( $_[0], $_[1] ) } );
Perlito5::Precedence::add_term( 'grep'  => sub { Perlito5::Grammar::Map->term_map_or_sort( $_[0], $_[1] ) } );

token map_or_sort { 'map' | 'sort' | 'grep' };

token term_map_or_sort {
    # Note: this is map-block; map-expr is parsed as a normal subroutine
    <map_or_sort> <.Perlito5::Grammar::Space.opt_ws> <before '{'> <Perlito5::Expression.term_curly> 
        <Perlito5::Expression.list_parse>
        {
            $MATCH->{capture} = [ 'term',
                 Perlito5::AST::Apply->new(
                    code      => Perlito5::Match::flat($MATCH->{map_or_sort}),
                    arguments => [
                        Perlito5::AST::Lit::Block->new( stmts => $MATCH->{'Perlito5::Expression.term_curly'}{capture}[2] ),
                        @{ Perlito5::Expression::expand_list($MATCH->{'Perlito5::Expression.list_parse'}{capture}) }
                    ], 
                    namespace => ''
                 )
               ]
        }
};

1;

