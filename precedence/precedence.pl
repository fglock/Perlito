
class Main {
    use MiniPerl6::Precedence;
    use MiniPerl6::Grammar;


    token paren_lexer { 
        | '+'   { make [ 'op', '+' ] }
        | '|'   { make [ 'op', '|' ] }
        | '&'   { make [ 'op', '&' ] }
        | <MiniPerl6::Grammar.ident> 
                { make [ 'term', ~$<MiniPerl6::Grammar.ident> ] }
        | ' '+  { make [ 'space', ' ' ] }
        | ')'   { make [ 'end', ')' ] }
    }
    method paren_parse ($str, $pos) {
        say "paren_parse ",$str," at ",$pos;
        my $expr;
        my $last_pos = $pos;
        my $get_token = sub {
            my $m = self.paren_lexer($str, $last_pos);
            if !$m {
                return undef;
            }
            my $v = $$m;
            $last_pos = $m.to;
            say "paren_lexer " ~ $v.perl;
            return $v;
        };
        my $res = MiniPerl6::Precedence::precedence_parse($get_token);
        say $res.perl;
        return MiniPerl6::Match.new( 
            'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1, 
            capture => $res);
    }


    token lexer { 
        | '+'   { make [ 'op', '+' ] }
        | '|'   { make [ 'op', '|' ] }
        | '&'   { make [ 'op', '&' ] }
        | <MiniPerl6::Grammar.ident> 
                { make [ 'term', ~$<MiniPerl6::Grammar.ident> ] }
        | '(' <paren_parse>
                { make [ 'term', $$<paren_parse> ] }
        | ' '+  { make [ 'space', ' ' ] }
        | ';'   { make [ 'end', ';' ] }
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
        my $res = MiniPerl6::Precedence::precedence_parse($get_token);
        say $res.perl;
        return MiniPerl6::Match.new( 
            'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1, 
            capture => $res)
    } 
    my $s = '; a|b| (c+y) & x ;...';
    my $res = Main.exp_parse( $s, 1 );
    say ($$res).perl;
    say "from: ", $res.from, " to: ", $res.to, " tail: ", substr($s, $res.to);

}

