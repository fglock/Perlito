
class Main {
    use MiniPerl6::Precedence;

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

    $expr = [ '1', '??', '$a', '*', '$b', '!!', '9' ];
    $last_pos = 0;
    say "expr: " ~ $expr.perl;
    my $res = MiniPerl6::Precedence::precedence_parse($get_token, '');
    say $res.perl;
    say "expr at ", $last_pos, " '", $expr[$last_pos], "'";


    $expr = [ '(', '3', '**', '4', '**', '6', ')' ];
    $last_pos = 0;
    say "expr: " ~ $expr.perl;
    my $res = MiniPerl6::Precedence::precedence_parse($get_token, '');
    say $res.perl;
    say "expr at ", $last_pos, " '", $expr[$last_pos], "'";

    $expr = [ '$a', '[', '4', ']', '.', 'meth' ];
    $last_pos = 0;
    say "expr: " ~ $expr.perl;
    my $res = MiniPerl6::Precedence::precedence_parse($get_token, '');
    say $res.perl;
    say "expr at ", $last_pos, " '", $expr[$last_pos], "'";

    $expr = [ ' ', '$a', '[', '4', ']', '.', 'meth', '(', '123', ')' ];
    $last_pos = 0;
    say "expr: " ~ $expr.perl;
    my $res = MiniPerl6::Precedence::precedence_parse($get_token, '');
    say $res.perl;
    say "expr at ", $last_pos, " '", $expr[$last_pos], "'";

    $expr = [ '.', 'meth', '(', '123', ')' ];
    $last_pos = 0;
    say "expr: " ~ $expr.perl;
    my $res = MiniPerl6::Precedence::precedence_parse($get_token, '');
    say $res.perl;
    say "expr at ", $last_pos, " '", $expr[$last_pos], "'";

    # TODO - test .() .[] .{}
    $expr = [ '$v', '.', '[' , '0', ',', '1', ',', ',', '2', ',', '3', ',', ']' ];
    $last_pos = 0;
    say "expr: " ~ $expr.perl;
    my $res = MiniPerl6::Precedence::precedence_parse($get_token, '');
    say $res.perl;
    say "expr at ", $last_pos, " '", $expr[$last_pos], "'";

    $expr = [ '$v', '.', '[' , '|', '1', '|', '2', '|', '3', '|', '4', ']' ];
    $last_pos = 0;
    say "expr: " ~ $expr.perl;
    my $res = MiniPerl6::Precedence::precedence_parse($get_token, '');
    say $res.perl;
    say "expr at ", $last_pos, " '", $expr[$last_pos], "'";

}

