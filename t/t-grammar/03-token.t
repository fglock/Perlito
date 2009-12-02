use v6-alpha;

use MiniPerl6::Grammar;
use MiniPerl6::Grammar::Regex;
use MiniPerl6::Emitter::Token;

{
    # simple constant 
    my $p = MiniPerl6::Grammar::Regex.term( 'a' );
    say ($$p).perl;
    say ($$p).emit;
}

# TODO 
#{
#    # hash dispatch
#    my $p = MiniPerl6::Grammar::Regex.term( '<%h>' );
#    say ($$p).perl;
#    say ($$p).emit;
#}

{
    # no quantifier
    my $p = MiniPerl6::Grammar::Regex.quantifier( 'b' );
    say ($$p).perl;
    say ($$p).emit;
}

{
    # no concat
    my $p = MiniPerl6::Grammar::Regex.concat( 'c' );
    say ($$p).perl;
    say ($$p).emit;
}

{
    # concat
    my $p = MiniPerl6::Grammar::Regex.concat( 'def' );
    say ($$p).perl;
    say ($$p).emit;
}

{
    # or
    my $p = MiniPerl6::Grammar::Regex.rule( 'de|f' );
    say ($$p).perl;
    say ($$p).emit;
}

{
    # subrule
    my $p = MiniPerl6::Grammar::Regex.rule( '<xyz>' );
    say ($$p).perl;
    say ($$p).emit;
}

{
    # meta constant
    my $p = MiniPerl6::Grammar::Regex.rule( "<'xyz'>" );
    say ($$p).perl;
    say ($$p).emit;
}

{
    # meta constant
    my $p = MiniPerl6::Grammar::Regex.rule( "<''>" );
    say ($$p).perl;
    say ($$p).emit;
}

{
    # escaped char
    my $p = MiniPerl6::Grammar::Regex.rule( '\\n\d\.\x' );
    say ($$p).perl;
    say ($$p).emit;
}

{
    # non-siginificant spaces
    my $p = MiniPerl6::Grammar::Regex.rule( 'a b c' );
    say ($$p).perl;
    say ($$p).emit;
}

{
    # end-of-regex
    my $p = MiniPerl6::Grammar::Regex.rule( 'a /' );
    say ($$p).perl;
    say ($$p).emit;
    $p = MiniPerl6::Grammar::Regex.rule( 'a }' );
    say ($$p).perl;
    say ($$p).emit;
}

=pod 

{
    # simple variable
    my $p = MiniPerl6::Grammar::Regex.term( '$1' );
    say ($$p).perl;
    say ($$p).emit;
}

=cut

