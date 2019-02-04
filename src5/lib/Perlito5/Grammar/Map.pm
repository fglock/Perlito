
package Perlito5::Grammar::Map;

use strict;

token map_or_grep { 'map' | 'grep' };


token term_map_or_grep {
    # Note: this is map-block; map-expr is parsed as a normal subroutine
    <map_or_grep> <.Perlito5::Grammar::Space::opt_ws>
    [
        <Perlito5::Grammar::block> 
        <Perlito5::Grammar::Expression::list_parse>
        {
            $MATCH->{capture} = [ 'term',
                 Perlito5::AST::Apply->new(
                    code        => Perlito5::Match::flat($MATCH->{map_or_grep}),
                    special_arg => $MATCH->{'Perlito5::Grammar::block'}{capture},
                    arguments   => Perlito5::Grammar::Expression::expand_list($MATCH->{'Perlito5::Grammar::Expression::list_parse'}{capture}), 
                    namespace   => ''
                 )
               ]
        }
    |
        '(' <.Perlito5::Grammar::Space::opt_ws>
        <Perlito5::Grammar::block> 
        <Perlito5::Grammar::Expression::list_parse>
        ')'
        {
            $MATCH->{capture} = [ 'term',
                 Perlito5::AST::Apply->new(
                    code        => Perlito5::Match::flat($MATCH->{map_or_grep}),
                    special_arg => $MATCH->{'Perlito5::Grammar::block'}{capture},
                    arguments   => Perlito5::Grammar::Expression::expand_list($MATCH->{'Perlito5::Grammar::Expression::list_parse'}{capture}), 
                    namespace   => ''
                 )
               ]
        }
    ]
};

#     Warning: syntactical care is required when sorting the list
#     returned from a function. If you want to sort the list returned
#     by the function call "find_records(@key)", you can use:
# 
#         @contact = sort { $a cmp $b } find_records @key;
#         @contact = sort +find_records(@key);
#         @contact = sort &find_records(@key);
#         @contact = sort(find_records(@key));
# 
#     If instead you want to sort the array @key with the comparison
#     routine "find_records()" then you can use:
# 
#         @contact = sort { find_records() } @key;
#         @contact = sort find_records(@key);
#         @contact = sort(find_records @key);
#         @contact = sort(find_records (@key));

token non_core_ident {
     <Perlito5::Grammar::full_ident>
     {
         my $name = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::full_ident"});  # TODO - split namespace
        return
          if exists( $Perlito5::CORE_PROTO->{$name} )
          || Perlito5::is_core_sub($name);
     }
};

token term_sort {
    # Note: this is sort-block; sort-expr is parsed as a normal subroutine
    'sort' <.Perlito5::Grammar::Space::opt_ws>
    [
        '(' <.Perlito5::Grammar::Space::opt_ws>
        [
            # sort '(' <opt_ws> SUBNAME <ws> LIST ')'
            <Perlito5::Grammar::Map::non_core_ident>
            <.Perlito5::Grammar::Space::ws>      # must have space
            {
                my $name = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Map::non_core_ident"});  # TODO - split namespace
                $MATCH->{_tmp} = $name;
            }
        |
            # sort '(' <opt_ws> BLOCK LIST ')'
            <Perlito5::Grammar::block> 
            {
                $MATCH->{_tmp} = $MATCH->{'Perlito5::Grammar::block'}{capture};
            }
        |
            # sort '(' VAR LIST ')'
            <before '$'> <Perlito5::Grammar::Sigil::term_sigil>
            {
                my $var = Perlito5::Match::flat($MATCH->{'Perlito5::Grammar::Sigil::term_sigil'})->[1];
                return if ref($var) ne 'Perlito5::AST::Var';
                $MATCH->{_tmp} = $var;
            }
        ]
        <Perlito5::Grammar::Expression::list_parse>
        ')'
        {
            $MATCH->{capture} = [ 'term',
                 Perlito5::AST::Apply->new(
                    code        => 'sort',
                    special_arg => $MATCH->{_tmp},
                    arguments   => Perlito5::Grammar::Expression::expand_list($MATCH->{'Perlito5::Grammar::Expression::list_parse'}{capture}), 
                    namespace   => ''
                 )
               ]
        }
    |
        [
            # sort BLOCK LIST
            <Perlito5::Grammar::block> 
            {
                $MATCH->{_tmp} = $MATCH->{'Perlito5::Grammar::block'}{capture};
            }
        |
            # sort SUBNAME LIST
            <Perlito5::Grammar::Map::non_core_ident>
            {
                my $name = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Map::non_core_ident"});  # TODO - split namespace
                $MATCH->{_tmp} = $name; 
            }
        |
            # sort VAR LIST
            <before '$'> <Perlito5::Grammar::Sigil::term_sigil>
            {
                my $var = Perlito5::Match::flat($MATCH->{'Perlito5::Grammar::Sigil::term_sigil'})->[1];
                return if ref($var) ne 'Perlito5::AST::Var';
                $MATCH->{_tmp} = $var;
            }
        |
            # sort LIST
            {
                $MATCH->{_tmp} = Perlito5::AST::Block->new(
                    'stmts' => [ Perlito5::AST::Apply->new(
                                    'code' => 'infix:<cmp>',
                                    'arguments' => [
                                        Perlito5::AST::Var->new(
                                            name => 'a', namespace => $Perlito5::PKG, sigil => '$',
                                        ),
                                        Perlito5::AST::Var->new(
                                            name => 'b', namespace => $Perlito5::PKG, sigil => '$',
                                        ),
                                    ]
                                 ),
                               ],
                    );
            }
        ]
        <Perlito5::Grammar::Expression::list_parse>
        {
            $MATCH->{capture} = [ 'term',
                 Perlito5::AST::Apply->new(
                    code        => 'sort',
                    special_arg => $MATCH->{_tmp},
                    arguments => Perlito5::Grammar::Expression::expand_list($MATCH->{'Perlito5::Grammar::Expression::list_parse'}{capture}),
                    namespace => ''
                 )
               ]
        }
    ]
};


Perlito5::Grammar::Precedence::add_term( 'map'   => \&term_map_or_grep );
Perlito5::Grammar::Precedence::add_term( 'grep'  => \&term_map_or_grep );
Perlito5::Grammar::Precedence::add_term( 'sort'  => \&term_sort );

1;


