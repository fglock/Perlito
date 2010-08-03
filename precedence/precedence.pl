
class Main {
    use MiniPerl6::Precedence;
    use MiniPerl6::Grammar;

    my $expr;
    my $last_pos;
    
    my $get_token = sub {
        if $last_pos > @($expr) {
            return undef;
        }
        my $v = $expr[$last_pos];
        $last_pos = $last_pos + 1;
        say "get_token " ~ $v;
        return $v;
    };

    $expr = [ ['term','1'], ['op','??'], ['term','$a'], ['op','*'], ['term','$b'], ['op','!!'], ['term','9'] ];
    $last_pos = 0;
    say "expr: " ~ $expr.perl;
    my $res = MiniPerl6::Precedence::precedence_parse($get_token, '');
    say $res.perl;
    say "expr at ", $last_pos, " '", $expr[$last_pos], "'";


    token lexer { 
        | '|'   { make [ 'op', '|' ] }
        | '&'   { make [ 'op', '&' ] }
        | <MiniPerl6::Grammar.ident> 
                { make [ 'term', ~$<MiniPerl6::Grammar.ident> ] }
        | ' '+  { make [ 'space', ' ' ] }
    }
    method exp_parse ($str, $pos) {
        say "exp_parse ",$str," at ",$pos;
        my $expr;
        my $last_pos = $pos;
        my $get_token = sub {
            my $m = self.lexer($str, $last_pos);
            if !$m {
                return undef;
            }
            my $v = $$m;
            $last_pos = $m.to;
            say "lexer " ~ $v.perl;
            return $v;
        };
        my $res = MiniPerl6::Precedence::precedence_parse($get_token, '');
        say $res.perl;
        return MiniPerl6::Match.new( 
            'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1, 
            capture => $res)
    } 
    my $res = Main.exp_parse( 'a|b|c & x', 0 );
    say ($$res).perl;

}

