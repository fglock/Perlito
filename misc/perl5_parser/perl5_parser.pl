use strict;
use warnings;
use 5.034;
use utf8;
use Data::Dumper;

#
# tokenizer format is an array of arrays:
#
#    [
#      [ NUMBER,     '123' ],   # NUMBER, WHITESPACE are numeric constants
#      [ WHITESPACE, ' '   ],
#      [ OPERATOR,   '+'   ],
#      ...
#      [ END_TOKEN,  '' ],
#    ]
#
# AST format is a hash:
#
#    {
#      'index' => 520,           # first token id (in the @$tokens array)
#      'next'  => 521,           # next token id
#      'type'  => 'INTEGER',     # AST object type (INTEGER, STRING, ...)
#      'value' => '3'            # AST contents can be STRING, ARRAY or HASH
#    }
#
# in case of parsing error, there are 2 possibilities:
#
# - die() is called with the error message;
#
# - or an AST is returned that looks like:
#
#    { FAIL => 1, index => 520 }
#
#   why returning FAIL instead of die(): it allows for backtracking and for error handling in a different place.
#   but eventually I'd rather get rid of all backtracking
#

## # uncomment to debug autovivification
## package TieArrayNoAutovivification {
##     use Tie::Array;
##     use parent qw(Tie::Array);
##     use Carp;
##     our @ISA = ('Tie::StdArray');
##
##     sub FETCH {
##         my ( $array, $key ) = @_;
##         Carp::confess "key '$key' does not exist" unless exists $array->[$key];
##         return $array->[$key];
##     }
## }

use constant {
    START      => 0,
    END_TOKEN  => 1,
    WHITESPACE => 2,
    KEYWORD    => 3,
    IDENTIFIER => 4,
    NUMBER     => 5,
    OPERATOR   => 9,
    STRING     => 11,

    STRING_DELIM  => 12,
    NEWLINE       => 13,
    START_COMMENT => 14,
    ESCAPE        => 15,
    MINUS         => 16,
    DOT           => 17,
    SIGIL         => 18,
    PAREN_OPEN    => 19,
    PAREN_CLOSE   => 20,
    QUESTION      => 21,
    COLON         => 22,
    COMMA         => 23,
    SQUARE_OPEN   => 24,
    SQUARE_CLOSE  => 25,
    CURLY_OPEN    => 26,
    CURLY_CLOSE   => 27,
    SEMICOLON     => 28,
    ARROW         => 29,
    EQUALS        => 30,
    SLASH         => 31,
    FAT_ARROW     => 32,
    DOUBLE_COLON  => 33,
    LESS_THAN     => 34,
};

my %TOKEN_NAME = (
    END_TOKEN()     => 'END_TOKEN',
    WHITESPACE()    => 'WHITESPACE',
    KEYWORD()       => 'KEYWORD',
    IDENTIFIER()    => 'IDENTIFIER',
    NUMBER()        => 'NUMBER',
    OPERATOR()      => 'OPERATOR',
    STRING()        => 'STRING',
    STRING_DELIM()  => 'STRING_DELIM',
    NEWLINE()       => 'NEWLINE',
    START_COMMENT() => 'START_COMMENT',
    ESCAPE()        => 'ESCAPE',
    MINUS()         => 'MINUS',
    DOT()           => 'DOT',
    SIGIL()         => 'SIGIL',
    PAREN_OPEN()    => 'PAREN_OPEN',
    PAREN_CLOSE()   => 'PAREN_CLOSE',
    QUESTION()      => 'QUESTION',
    COLON()         => 'COLON',
    COMMA()         => 'COMMA',
    SQUARE_OPEN()   => 'SQUARE_OPEN',
    SQUARE_CLOSE()  => 'SQUARE_CLOSE',
    CURLY_OPEN()    => 'CURLY_OPEN',
    CURLY_CLOSE()   => 'CURLY_CLOSE',
    SEMICOLON()     => 'SEMICOLON',
    ARROW()         => 'ARROW',
    EQUALS()        => 'EQUALS',
    SLASH()         => 'SLASH',
    FAT_ARROW()     => 'FAT_ARROW',
    DOUBLE_COLON()  => 'DOUBLE_COLON',
    LESS_THAN()     => 'LESS_THAN',
);

my %OPERATORS = (
    ','  => COMMA(),
    '#'  => START_COMMENT(),
    "'"  => STRING_DELIM(),
    '"'  => STRING_DELIM(),
    '`'  => STRING_DELIM(),
    '\\' => ESCAPE(),
    '-'  => MINUS(),
    '.'  => DOT(),
    '$'  => SIGIL(),
    '$#' => SIGIL(),
    '@'  => SIGIL(),
    '%'  => SIGIL(),
    '('  => PAREN_OPEN(),
    ')'  => PAREN_CLOSE(),
    '?'  => QUESTION(),
    ':'  => COLON(),
    '['  => SQUARE_OPEN(),
    ']'  => SQUARE_CLOSE(),
    '{'  => CURLY_OPEN(),
    '}'  => CURLY_CLOSE(),
    ';'  => SEMICOLON(),
    '->' => ARROW(),
    '='  => EQUALS(),
    '/'  => SLASH(),
    '=>' => FAT_ARROW(),
    '::' => DOUBLE_COLON(),
    '<'  => LESS_THAN(),
    map { $_ => OPERATOR() }
      qw(
      == != <= >= > <=>
      =~ !~
      + * ** % ++ -- && || // ! ^ ~ ~~ & |
      >> <<
      **= += -= *= /= x= |= &= .= <<= >>= %= ||= &&= ^= //=
      |.= &.= ^.=
      )
);

# tokenize()
#
# known problems:
#   10E10     tokenizes to 10,E10
#   q!=!=="=" tokenizes to q,!=,!=, ...
#
sub tokenize {
    my ($code) = @_;
    my $state = START();
    my @tokens;
    my $buffer;

    #  tie @tokens, 'TieArrayNoAutovivification';   # uncomment to debug autovivification

  FSM:
    for my $char ( split //, $code ) {
        if ( $state == START() ) {
            if ( $char =~ /\s/ ) {
                if ( $char eq "\n" ) {
                    push @tokens, [ NEWLINE(), $char ];
                    next FSM;
                }
                $state = WHITESPACE();
            }
            elsif ( $char =~ /[a-zA-Z_]/ ) {
                $state = IDENTIFIER();
            }
            elsif ( $char =~ /[0-9]/ ) {
                $state = NUMBER();
            }
            elsif ( exists $OPERATORS{$char} ) {
                $state = OPERATOR();
            }
            else {
                push @tokens, [ STRING(), $char ];
                next FSM;
            }
            $buffer = $char;
        }
        elsif ( $state == WHITESPACE() ) {
            if ( $char !~ /\s/ || $char eq "\n" ) {
                push @tokens, [ WHITESPACE(), $buffer ];
                $state = START();
                redo FSM;
            }
            else {
                $buffer .= $char;
            }
        }
        elsif ( $state == IDENTIFIER() ) {
            if ( $char !~ /[a-zA-Z0-9_]/ ) {
                push @tokens, [ IDENTIFIER(), $buffer ];
                $state = START();
                redo FSM;
            }
            else {
                $buffer .= $char;
            }
        }
        elsif ( $state == NUMBER() ) {
            if ( $char !~ /[0-9]/ ) {
                push @tokens, [ NUMBER(), $buffer ];
                $state = START();
                redo FSM;
            }
            else {
                $buffer .= $char;
            }
        }
        elsif ( $state == OPERATOR() ) {
            if ( exists $OPERATORS{"$buffer$char"} ) {
                $buffer .= $char;
            }
            else {
                push @tokens, [ $OPERATORS{$buffer}, $buffer ];
                $state = START();
                redo FSM;
            }
        }
    }
    if ( $buffer ne '' && $state != START() ) {
        if ( $state == OPERATOR() ) {
            push @tokens, [ $OPERATORS{$buffer}, $buffer ];
        }
        else {
            push @tokens, [ $state, $buffer ];
        }
    }
    push @tokens, [ END_TOKEN(), '' ];
    push @tokens, [ END_TOKEN(), '' ];
    push @tokens, [ END_TOKEN(), '' ];
    return \@tokens;
}

my $LIST_OPERATOR_PRECEDENCE = 4;
my %PRECEDENCE               = (
    'or'  => 1,
    'xor' => 1,
    'and' => 2,
    'not' => 3,    # Unary negation

    # $LIST_OPERATOR_PRECEDENCE = 4

    ','  => 5,
    '=>' => 5,

    '='   => 6,
    '+='  => 6,
    '-='  => 6,
    '*='  => 6,
    '/='  => 6,
    '.='  => 6,
    'x='  => 6,
    '%='  => 6,
    '**=' => 6,

    '?' => 7,    # Ternary operator

    '||' => 11,
    '&&' => 12,

    '=='  => 13,
    '!='  => 13,
    '<=>' => 13,

    '<'  => 14,
    '>'  => 14,
    '<=' => 14,
    '>=' => 14,

    '+' => 15,
    '-' => 15,
    '.' => 15,    # String concatenation

    '*' => 16,
    '/' => 16,
    '%' => 16,
    'x' => 16,    # String repetition

    '=~' => 17,
    '!~' => 17,

    '!'  => 18,    # Unary negation
    '\\' => 18,    # create reference

    '**' => 19,

    '++' => 20,
    '--' => 20,

    '->' => 21,
    '('  => 21,    # function call
    '{'  => 21,    # hash element
    '['  => 21,    # array element

    '$'  => 22,
    '$#' => 22,
    '@'  => 22,
    '%'  => 22,
);

my %LIST = (
    ','  => 1,
    '=>' => 1,
);
my %PREFIX = (
    '!'   => 1,
    '\\'  => 1,
    'not' => 1,
    '-'   => 1,
    '+'   => 1,
    '--'  => 1,
    '++'  => 1,
    '$'   => 1,
    '$#'  => 1,
    '@'   => 1,
    '%'   => 1,
);
my %POSTFIX = (
    '--' => 1,
    '++' => 1,
);

# default associativity is LEFT
my %ASSOC_RIGHT = (
    '**'  => 1,
    '='   => 1,
    '+='  => 1,
    '-='  => 1,
    '*='  => 1,
    '/='  => 1,
    '.='  => 1,
    'x='  => 1,
    '%='  => 1,
    '**=' => 1,
    ','   => 1,
    '=>'  => 1,
);
my %NON_ASSOC_AUTO = (
    '++' => 1,
    '--' => 1,
);

sub error_message_string_terminator {
    my ( $tokens, $index, $quote ) = @_;
    if ( $quote eq '"' ) {
        return "Can't find string terminator '$quote' anywhere before EOF\n";
    }
    return "Can't find string terminator \"$quote\" anywhere before EOF\n";
}

sub error_message {
    my ( $tokens, $index, $message ) = @_;

    # - adds location context to error messages
    # - also adds a newline
    #
    #   Bareword found where operator expected (Missing operator before "a"?) at -e line 1, near "2 a"
    #   syntax error at -e line 1, near "--for "
    #
    my $line        = 1;
    my $first_token = 1;
    for ( 0 .. $index ) {    # retrieve line number
        if ( $tokens->[$_][0] == NEWLINE ) {
            $line++;
            $first_token = $_;
        }
    }
    my @near;                # retrieve string context
    for ( $index - 2 .. $index + 2 ) {
        if ( $index >= 0 && $index < $#$tokens ) {
            push @near, $tokens->[$_][1];
        }
    }

    ## my $col  = 1;    # retrieve column number
    ## for ( $first_token .. $index ) {
    ##     $col += length($tokens->[$_][1]);
    ## }
    return $message . ' at line ' . $line . ', near "' . join( '', @near ) . '"' . "\n";
}

# parse_precedence_expression()
#
# this is a standard precedence parser, but special cases were added
#
# these have the usual meaning:
#
#   PREFIX          like:  (++a)
#   POSTFIX         like:  (a++)
#   INFIX_LEFT      like:  (a || b)  execute a first
#   INFIX_RIGHT     like:  (a = b)   execute b first
#
# these are the extra types:
#
#   POSTFIX_TERM    like:  (a[1])
#   LIST_OP         the operator can be anywhere: prefix, infix, postfix, and repeated:  (,,a => b, c =>)
#   NON_ASSOC       operators of the same type cannot be repeated; these are forbidden:  (-- --a); (++a--)
#   TERNARY         like:  (a ? b : c)
#
sub parse_precedence_expression {
    my ( $tokens, $index, $min_precedence ) = @_;

    # Handle unary operators
    my $pos      = $index;
    my $op_value = $tokens->[$pos][1];
    my $left_expr;
    if ( $PREFIX{$op_value} || $LIST{$op_value} ) {
        $pos++;
        $pos = parse_optional_whitespace( $tokens, $pos )->{next};
        my $expr = parse_precedence_expression( $tokens, $pos, $PRECEDENCE{$op_value} );
        if ( $expr->{FAIL} ) {

            # backtrack
            if ( $LIST{$op_value} ) {

                # Handle a lone comma and fat comma without any value
                return { type => 'LIST_OP', value => [$op_value], next => $pos };
            }
            return parse_fail( $tokens, $index );
        }
        if ( $LIST{$op_value} ) {

            # Handle prefix comma and fat comma
            my @left = ($expr);
            if ( $expr->{type} eq 'LIST_OP' ) {
                @left = @{ $expr->{value} };
            }
            $left_expr = { type => 'LIST_OP', value => [ $op_value, @left ], next => $expr->{next} };
        }
        else {

            if (   $NON_ASSOC_AUTO{$op_value}
                && ( $expr->{type} eq 'POSTFIX_OP' || $expr->{type} eq 'PREFIX_OP' )
                && $NON_ASSOC_AUTO{ $expr->{value}{op} } )    # check for nonassoc syntax error
            {
                die error_message( $tokens, $index, "syntax error" );
            }

            $left_expr = { type => 'PREFIX_OP', value => { op => $op_value, arg => $expr }, next => $expr->{next} };
        }
        $pos = parse_optional_whitespace( $tokens, $left_expr->{next} )->{next};
        if ( $tokens->[$pos][0] == END_TOKEN() ) {
            return $left_expr;
        }
    }
    else {
        $left_expr = parse_term( $tokens, $index );
        if ( $left_expr->{FAIL} ) {
            return parse_fail( $tokens, $index );
        }
        $pos = $left_expr->{next};
        $pos = parse_optional_whitespace( $tokens, $left_expr->{next} )->{next};
        if ( $tokens->[$pos][0] == END_TOKEN() ) {
            return $left_expr;
        }
    }

    $pos = $left_expr->{next};
    while ( $tokens->[$pos][0] != END_TOKEN() ) {
        $pos = parse_optional_whitespace( $tokens, $pos )->{next};
        return parse_fail( $tokens, $index ) if $tokens->[$pos][0] == END_TOKEN();
        my $op_value = $tokens->[$pos][1];
        my $type     = $tokens->[$pos][0];
        my $op_pos   = $pos;

        last unless exists $PRECEDENCE{$op_value};
        my $precedence = $PRECEDENCE{$op_value};
        last if $precedence < $min_precedence;

        $pos++;
        $pos = parse_optional_whitespace( $tokens, $pos )->{next};

        # Handle postfix () [] {}
        if ( $type == PAREN_OPEN() || $type == CURLY_OPEN() || $type == SQUARE_OPEN() ) {
            my $right_expr = parse_term( $tokens, $op_pos );
            if ( $right_expr->{FAIL} ) {
                return parse_fail( $tokens, $index );
            }
            $left_expr = { type => 'APPLY_OR_DEREF', value => [ $left_expr, $right_expr ], next => $right_expr->{next} };
            $pos       = $left_expr->{next};
            next;
        }

        # Handle ternary operator
        if ( $type == QUESTION() ) {
            my $true_expr = parse_precedence_expression( $tokens, $pos, 0 );    # Parse the true branch
            if ( $true_expr->{FAIL} ) {
                return parse_fail( $tokens, $index );
            }
            $pos = $true_expr->{next};
            $pos = parse_optional_whitespace( $tokens, $pos )->{next};
            if ( $tokens->[$pos][0] == COLON() ) {
                $pos++;    # Consume the ':'
                $pos = parse_optional_whitespace( $tokens, $pos )->{next};
            }
            else {
                return parse_fail( $tokens, $index );
            }
            my $false_expr = parse_precedence_expression( $tokens, $pos, $PRECEDENCE{'?'} );    # Parse the false branch
            $left_expr = { type => 'TERNARY_OP', value => [ '?', $left_expr, $true_expr, $false_expr ], next => $false_expr->{next} };
            $pos       = $left_expr->{next};
            next;
        }

        # Handle postfix operators
        if ( $POSTFIX{$op_value} ) {

            if (   $NON_ASSOC_AUTO{$op_value}
                && ( $left_expr->{type} eq 'POSTFIX_OP' || $left_expr->{type} eq 'PREFIX_OP' )
                && $NON_ASSOC_AUTO{ $left_expr->{value}{op} } )    # check for nonassoc syntax error
            {
                die error_message( $tokens, $index, "syntax error" );
            }

            $left_expr = { type => 'POSTFIX_OP', value => { op => $op_value, arg => $left_expr }, next => $pos };
            next;
        }

        my $next_min_precedence = $ASSOC_RIGHT{$op_value} ? $precedence : $precedence + 1;
        my $right_expr          = parse_precedence_expression( $tokens, $pos, $next_min_precedence );
        if ( $right_expr->{FAIL} ) {

            # backtrack
            if ( $LIST{$op_value} ) {

                # Handle terminal comma and fat comma
                my @left = ($left_expr);
                if ( $left_expr->{type} eq 'LIST_OP' ) {
                    @left = @{ $left_expr->{value} };
                }
                return { type => 'LIST_OP', value => [ @left, $op_value ], next => $pos };
            }
            else {
                return parse_fail( $tokens, $index );
            }
        }

        if ( $LIST{$op_value} ) {

            # Handle list separators (comma and fat comma)
            my @right = ($right_expr);
            if ( $right_expr->{type} eq 'LIST_OP' ) {
                @right = @{ $right_expr->{value} };
            }
            $left_expr = { type => 'LIST_OP', value => [ $left_expr, $op_value, @right ], next => $right_expr->{next} };
        }
        else {
            $left_expr = { type => 'BINARY_OP', value => [ $op_value, $left_expr, $right_expr ], next => $right_expr->{next} };
        }
        $pos = $left_expr->{next};
        $pos = parse_optional_whitespace( $tokens, $pos )->{next};
    }
    return $left_expr;
}

sub parse_fail {
    my ( $tokens, $index ) = @_;
    return { FAIL => 1, index => $index };
}

sub parse_optional_whitespace {
    my ( $tokens, $index ) = @_;
    my $pos = $index;
  WS:
    while ( $tokens->[$pos][0] != END_TOKEN() ) {
        if ( $tokens->[$pos][0] == WHITESPACE() ) {
            $pos++;
        }
        if ( $tokens->[$pos][0] == NEWLINE() ) {
            $pos++;
            if ( $tokens->[$pos][0] == EQUALS() ) {    # =pod
                if ( $tokens->[ $pos + 1 ][0] == IDENTIFIER() ) {
                    $pos++;
                    if ( $tokens->[$pos][1] eq 'encoding' ) {    # =encoding ... until end of line
                        $pos++;
                        while ( $tokens->[$pos][0] != NEWLINE() && $tokens->[$pos][0] != END_TOKEN() ) {
                            $pos++;
                        }
                    }
                    elsif ( $tokens->[$pos][1] eq 'for' ) {      # =for ... until end of paragraph
                        $pos++;
                        while ($tokens->[$pos][0] != NEWLINE() && $tokens->[$pos][0] != END_TOKEN()
                            || $tokens->[ $pos + 1 ][0] != NEWLINE() )
                        {
                            $pos++;
                        }
                        $pos++;
                    }
                    else {                                       # =any_command ... until =cut or =end
                        $pos++;
                        while ($tokens->[$pos][0] != NEWLINE() && $tokens->[$pos][0] != END_TOKEN()
                            || $tokens->[ $pos + 1 ][1] ne '='   && $tokens->[ $pos + 1 ][0] != END_TOKEN()
                            || $tokens->[ $pos + 2 ][1] ne 'cut' && $tokens->[ $pos + 2 ][1] ne 'end' )
                        {
                            $pos++;
                        }
                        $pos += 3;
                    }
                }
            }
            redo WS;
        }
        if ( $tokens->[$pos][0] == START_COMMENT() ) {
            $pos++;
            while ( $tokens->[$pos][0] != NEWLINE() && $tokens->[$pos][0] != END_TOKEN() ) {
                $pos++;
            }
            redo WS;
        }
        if ( $tokens->[$pos][0] == IDENTIFIER() && ( $tokens->[$pos][1] eq "__END__" || $tokens->[$pos][1] eq "__DATA__" ) ) {
            $pos = $#$tokens;
        }
        last WS;
    }
    return { type => 'WHITESPACE', index => $index, value => ' ', next => $pos };
}

sub parse_colon_bareword {
    my ( $tokens, $index ) = @_;
    my $pos = $index;
    my @tok;
    while ( $tokens->[$pos][0] != END_TOKEN()
        && ( $tokens->[$pos][0] == DOUBLE_COLON() || $tokens->[$pos][0] == IDENTIFIER() || $tokens->[$pos][0] == NUMBER() ) )
    {
        push @tok, $tokens->[$pos][1];
        $pos++;
    }
    if ( !@tok ) {
        return parse_fail( $tokens, $index );
    }
    return { type => 'COLON_BAREWORD', index => $index, value => \@tok, next => $pos };
}

sub parse_variable_interpolation_in_string {
    my ( $tokens, $index ) = @_;

    # "$a"  "${a}"  "$x[10]"  "$x->[10]"  "@{[ 1 + 1]}"  "$#a"
    my $pos = $index;
    if ( $tokens->[$pos][0] == SIGIL() ) {
        my $sigil = $tokens->[$pos][1];    # $
        $pos = parse_optional_whitespace( $tokens, $pos + 1 )->{next};
        my $expr;
        if ( $tokens->[$pos][0] == IDENTIFIER() || $tokens->[$pos][0] == NUMBER() || $tokens->[$pos][0] == DOUBLE_COLON() ) {

            # TODO special vars $$
            $expr = parse_colon_bareword( $tokens, $pos );
            if ( $expr->{FAIL} ) {
                return parse_fail( $tokens, $index );
            }

            # TODO if sigil is not $#, check for [] {} ->
        }
        elsif ( $tokens->[$pos][0] == CURLY_OPEN() ) {
            $expr = parse_delim_expression( $tokens, $pos, '{' );
            if ( $expr->{FAIL} ) {
                return parse_fail( $tokens, $index );
            }
        }
        return { type => 'PREFIX_OP', value => { op => $sigil, arg => $expr }, next => $expr->{next} };
    }
    return parse_fail( $tokens, $index );
}

sub parse_number {
    my ( $tokens, $index ) = @_;
    my $pos = $index;
    if ( $tokens->[$pos][0] == DOT() ) {
        $pos++;    # .
        if ( $tokens->[$pos][0] == NUMBER() ) {
            $pos++;    # .123
        }
        else {
            return parse_fail( $tokens, $index );
        }
    }
    elsif ( $tokens->[$pos][0] == NUMBER() ) {
        $pos++;    # 123
        if ( $tokens->[$pos][0] == DOT() ) {
            $pos++;    # 123.
            if ( $tokens->[$pos][0] == NUMBER() ) {
                $pos++;    # 123.456
            }
        }
        else {
            if ( $tokens->[$pos][0] != IDENTIFIER() ) {
                return { type => 'INTEGER', index => $index, value => join( '', map { $tokens->[$_][1] } $index .. $pos - 1 ), next => $pos };
            }
        }
    }
    else {
        return parse_fail( $tokens, $index );
    }

    if ( $tokens->[$pos][0] == IDENTIFIER() && $tokens->[$pos][1] =~ /^e([0-9]*)$/i ) {
        if ($1) {
            $pos++;    # E10
        }
        else {
            $pos++;    # 123E-10
            if ( $tokens->[$pos][0] == MINUS() ) {
                $pos++;    # -
            }
            if ( $tokens->[$pos][0] == NUMBER() ) {
                $pos++;    # 123
            }
            else {
                return parse_fail( $tokens, $index );
            }
        }
    }
    return { type => 'NUMBER', index => $index, value => join( '', map { $tokens->[$_][1] } $index .. $pos - 1 ), next => $pos };
}

my %ESCAPE_SEQUENCE = qw/ a 7 b 8 e 27 f 12 n 10 r 13 t 9 /;

my %QUOTE_PAIR = (
    '{' => '}',
    '(' => ')',
    '[' => ']',
    '<' => '>',
);

sub parse_string_delimiter_fixup {
    my ( $tokens, $index ) = @_;
    my $delim = $tokens->[$index][1];
    if ( length($delim) > 1 ) {    #  delimiter looks like: !=
        $delim               = substr( $delim, 0, 1 );
        $tokens->[$index][1] = substr( $delim, 1 );
        $index--;
    }
    if ( $QUOTE_PAIR{$delim} ) { $delim = $QUOTE_PAIR{$delim} }    # q< ... >
    return ( $delim, $index + 1 );
}

sub parse_single_quote_string {    # 'abc'
    my ( $tokens, $index, $pos ) = @_;
    my $quote;
    ( $quote, $pos ) = parse_string_delimiter_fixup( $tokens, $pos );
    my $value;
    while ( $tokens->[$pos][0] != END_TOKEN() ) {
        if ( $quote eq $tokens->[$pos][1] ) {
            return { type => 'STRING', index => $index, value => $value, next => $pos + 1 };
        }
        if ( $tokens->[$pos][0] == ESCAPE() ) {
            if ( $tokens->[ $pos + 1 ][1] eq $quote || $tokens->[ $pos + 1 ][0] == ESCAPE() ) {
                $pos++;
            }
        }
        $value .= $tokens->[$pos][1];
        $pos++;
    }
    die error_message_string_terminator( $tokens, $index, $quote );
}

sub parse_double_quote_string {    # "abc"
    my ( $tokens, $index, $pos ) = @_;
    my $quote;
    ( $quote, $pos ) = parse_string_delimiter_fixup( $tokens, $pos );
    my @ops;
    my $value = '';
    while ( $tokens->[$pos][0] != END_TOKEN() ) {
        my $type = $tokens->[$pos][0];
        if ( $quote eq $tokens->[$pos][1] ) {
            if ( length($value) > 0 || @ops < 1 ) {
                push @ops, { type => 'STRING', index => $index, value => $value, next => $pos + 1 };
            }
            if ( @ops == 1 ) {
                return $ops[0];
            }
            return { type => 'JOIN', index => $index, value => [ '', @ops ], next => $pos + 1 };
        }
        if ( $type == ESCAPE() ) {
            if ( $tokens->[ $pos + 1 ][1] eq $quote || $tokens->[ $pos + 1 ][0] == ESCAPE() ) {
                $pos++;
            }
            my $c2 = $tokens->[ $pos + 1 ][1];
            if ( exists $ESCAPE_SEQUENCE{$c2} ) {
                $value .= chr( $ESCAPE_SEQUENCE{$c2} );
                $pos++;
                $pos++;
                next;
            }
        }
        if ( $type == SIGIL() && $tokens->[$pos][1] ne '%' ) {
            my $expr = parse_variable_interpolation_in_string( $tokens, $pos );
            if ( $expr->{FAIL} ) {
                return parse_fail( $tokens, $index );
            }
            push @ops, { type => 'STRING', index => $index, value => $value, next => $pos } if length($value);
            push @ops, $expr;
            $value = '';
            $pos   = $expr->{next};
            next;
        }
        $value .= $tokens->[$pos][1];
        $pos++;
    }
    die error_message_string_terminator( $tokens, $index, $quote );
}

sub parse_regex_string {    # /abc/
    my ( $tokens, $index, $pos ) = @_;
    my $quote;
    ( $quote, $pos ) = parse_string_delimiter_fixup( $tokens, $pos );
    my @ops;
    my $value = '';
    while ( $tokens->[$pos][0] != END_TOKEN() ) {
        my $type = $tokens->[$pos][0];
        if ( $quote eq $tokens->[$pos][1] ) {
            if ( length($value) > 0 || @ops < 1 ) {
                push @ops, { type => 'STRING', index => $index, value => $value, next => $pos + 1 };
            }
            if ( @ops == 1 ) {
                return $ops[0];
            }
            return { type => 'JOIN', index => $index, value => [ '', @ops ], next => $pos + 1 };
        }
        if ( $type == ESCAPE() ) {
            if ( $tokens->[ $pos + 1 ][1] eq $quote || $tokens->[ $pos + 1 ][0] == ESCAPE() ) {
                $pos++;
            }
            my $c2 = $tokens->[ $pos + 1 ][1];
            if ( exists $ESCAPE_SEQUENCE{$c2} ) {
                $value .= chr( $ESCAPE_SEQUENCE{$c2} );
                $pos++;
                $pos++;
                next;
            }
        }
        if ( $type == SIGIL() && ( $tokens->[$pos][1] eq '@' || $tokens->[$pos][1] eq '$' || $tokens->[$pos][1] eq '$#' ) ) {
            my $expr = parse_variable_interpolation_in_string( $tokens, $pos );
            if ( $expr->{FAIL} ) {
                return parse_fail( $tokens, $index );
            }
            push @ops, { type => 'STRING', index => $index, value => $value, next => $pos } if length($value);
            push @ops, $expr;
            $value = '';
            $pos   = $expr->{next};
            next;
        }
        $value .= $tokens->[$pos][1];
        $pos++;
    }
    die error_message( $tokens, $index, "Search pattern not terminated" );
}

sub parse_delim_expression {
    my ( $tokens, $index, $delim ) = @_;
    my $pos         = $index;
    my $start_delim = $delim;
    my $precedence  = 0;
    $precedence = $PRECEDENCE{$start_delim} + 1 if $start_delim eq '<';
    if ( $QUOTE_PAIR{$delim} ) { $delim = $QUOTE_PAIR{$delim} }    # q< ... >
    if ( $tokens->[$pos][1] eq $start_delim ) {
        $pos = parse_optional_whitespace( $tokens, $pos + 1 )->{next};
        if ( $tokens->[$pos][1] eq $delim ) {
            return { type => 'PAREN', value => [ $start_delim, ], next => $pos + 1 };    # empty
        }
        my $expr = parse_precedence_expression( $tokens, $pos, $precedence );
        if ( $expr->{FAIL} ) {
            return parse_fail( $tokens, $index );
        }
        $pos = $expr->{next};
        $pos = parse_optional_whitespace( $tokens, $pos )->{next};
        if ( $tokens->[$pos][1] eq $delim ) {
            return { type => 'PAREN', value => [ $start_delim, $expr ], next => $pos + 1 };
        }
    }
    return parse_fail( $tokens, $index );
}

sub parse_statement_block {
    my ( $tokens, $index ) = @_;
    my $pos = $index;
    if ( $tokens->[$pos][0] == CURLY_OPEN() ) {
        $pos++;
        $pos = parse_optional_whitespace( $tokens, $pos )->{next};
        my @expr;
        while ( $tokens->[$pos][0] != END_TOKEN() ) {
            my $expr = parse_statement( $tokens, $pos );
            if ( $expr->{FAIL} ) {

                # return parse_fail( $tokens, $index );
                last;
            }
            push @expr, $expr;
            $pos = $expr->{next};
        }
        $pos = parse_optional_whitespace( $tokens, $pos )->{next};
        if ( $tokens->[$pos][0] == CURLY_CLOSE() ) {
            $pos++;
            return { type => 'STATEMENT_BLOCK', value => \@expr, next => $pos };
        }
    }
    return parse_fail( $tokens, $index );
}

sub parse_term {
    my ( $tokens, $index ) = @_;
    my $type = $tokens->[$index][0];
    my $pos  = $index;
    my $ast;
    if ( $type == NUMBER() || $type == DOT() ) {
        $ast = parse_number( $tokens, $index );
    }
    elsif ( $type == IDENTIFIER() ) {

        if ( $tokens->[ $pos + 1 ][0] == DOUBLE_COLON() ) {
            return parse_colon_bareword( $tokens, $index );    # TODO parse special cases like CORE::print
        }

        my $stmt = $tokens->[$pos][1];
        $pos = parse_optional_whitespace( $tokens, $pos + 1 )->{next};
        if ( $tokens->[$pos][0] == FAT_ARROW() ) {             # bareword
            return { type => 'STRING', value => $tokens->[$index][1], next => $index + 1 };
        }
        elsif ( $stmt eq 'use' ) {                             # XXX special case just for testing!
            my $expr = parse_precedence_expression( $tokens, $pos, $LIST_OPERATOR_PRECEDENCE );
            if ( $expr->{FAIL} ) {
                return parse_fail( $tokens, $index );
            }
            return { type => 'APPLY', value => { stmt => $stmt, args => $expr }, next => $expr->{next} };
        }
        elsif ( $stmt eq 'print' ) {                           # XXX special case just for testing!
            my $expr = parse_precedence_expression( $tokens, $pos, $LIST_OPERATOR_PRECEDENCE );
            if ( $expr->{FAIL} ) {
                return parse_fail( $tokens, $index );
            }
            return { type => 'APPLY', value => { stmt => $stmt, args => $expr }, next => $expr->{next} };
        }
        elsif ( $stmt eq 'do' ) {                              # do BLOCK
            my $block = parse_statement_block( $tokens, $pos );
            if ( $block->{FAIL} ) {
                return parse_fail( $tokens, $index );
            }
            $ast = { type => 'DO_BLOCK', value => { stmt => $stmt, block => $block }, next => $block->{next} };
            return $ast;
        }
        elsif ( $stmt eq 'q' ) {                               # q!...!
            return parse_single_quote_string( $tokens, $index, $pos );
        }
        elsif ( $stmt eq 'qq' ) {                              # qq!...!
            return parse_double_quote_string( $tokens, $index, $pos );
        }
        elsif ( $stmt eq 'm' ) {                               # /.../
            $ast = parse_regex_string( $tokens, $index, $pos );
            if ( $ast->{FAIL} ) {
                return parse_fail( $tokens, $index );
            }
            $pos = $ast->{next};
            my $modifier = "";
            if ( $tokens->[$pos][0] == IDENTIFIER() ) {    #  regex modifiers
                $modifier = $tokens->[$pos][1];
                $pos++;
            }
            return { type => 'REGEX', index => $index, value => { args => $ast, modifier => $modifier }, next => $pos, };
        }
        elsif ( $stmt eq 'qw' ) {                          # qw/.../
            $ast = parse_single_quote_string( $tokens, $index, $pos );
            if ( $ast->{FAIL} ) {
                return parse_fail( $tokens, $index );
            }
            return {
                type  => 'QW',
                index => $index,
                value => [
                    map { { type => 'STRING', index => $index, value => $_, next => $pos + 1 } }
                      split( ' ', $ast->{value} )
                ],
                next => $ast->{next}
            };
        }
        $ast = { type => 'BAREWORD', value => $tokens->[$index][1], next => $index + 1 };
    }
    elsif ( $type == STRING_DELIM() ) {
        my $quote = $tokens->[$index][1];
        if ( $quote eq "'" ) {
            return parse_single_quote_string( $tokens, $index, $index );
        }
        elsif ( $quote eq '"' ) {
            return parse_double_quote_string( $tokens, $index, $index );
        }
        elsif ( $quote eq '`' ) {    # TODO not implemented
        }
        return parse_fail( $tokens, $index );
    }
    elsif ( $type == PAREN_OPEN() ) {
        $ast = parse_delim_expression( $tokens, $index, '(' );
    }
    elsif ( $type == SQUARE_OPEN() ) {
        $ast = parse_delim_expression( $tokens, $index, '[' );
    }
    elsif ( $type == CURLY_OPEN() ) {
        $ast = parse_delim_expression( $tokens, $index, '{' );
    }
    elsif ( $type == LESS_THAN() ) {
        $ast = parse_delim_expression( $tokens, $index, '<' );
    }
    elsif ( $type == SLASH() ) {    # /.../
        $ast = parse_regex_string( $tokens, $index, $index );
        if ( $ast->{FAIL} ) {
            return parse_fail( $tokens, $index );
        }
        $pos = $ast->{next};
        my $modifier = "";
        if ( $tokens->[$pos][0] == IDENTIFIER() ) {    #  regex modifiers
            $modifier = $tokens->[$pos][1];
            $pos++;
        }
        return { type => 'REGEX', index => $index, value => { args => $ast, modifier => $modifier }, next => $pos, };
    }
    elsif ( $type = DOUBLE_COLON() ) {
        $ast = parse_colon_bareword( $tokens, $index );
        $pos = $ast->{next};
    }
    else {
        return parse_fail( $tokens, $index );
    }
    return $ast;
}

sub parse_statement {
    my ( $tokens, $index ) = @_;
    my $pos = $index;
    $pos = parse_optional_whitespace( $tokens, $pos )->{next};
    if ( $tokens->[$pos][0] == END_TOKEN() ) {
        return parse_fail( $tokens, $index );
    }
    my $ast;
    if ( $tokens->[$pos][0] == SEMICOLON() ) {
        $pos++;    # semicolon
        return { type => 'STATEMENT', value => { stmt => 'empty_statement' }, next => $pos };
    }
    elsif ( $tokens->[$pos][0] == IDENTIFIER() ) {
        my $stmt = $tokens->[$pos][1];
        if ( $stmt eq 'if' || $stmt eq 'unless' || $stmt eq 'while' || $stmt eq 'until' ) {
            $pos++;
            $pos = parse_optional_whitespace( $tokens, $pos )->{next};
            my $expr = parse_delim_expression( $tokens, $pos, '(' );
            if ( $expr->{FAIL} ) {
                return parse_fail( $tokens, $index );
            }
            $pos = $expr->{next};
            $pos = parse_optional_whitespace( $tokens, $pos )->{next};
            my $block = parse_statement_block( $tokens, $pos );
            if ( $block->{FAIL} ) {
                return parse_fail( $tokens, $index );
            }
            $ast = { type => 'STATEMENT', value => { stmt => $stmt, condition => $expr, block => $block }, next => $block->{next} };
            $pos = $ast->{next};
        }
    }
    elsif ( $tokens->[$pos][0] == CURLY_OPEN() ) {
        $ast = parse_statement_block( $tokens, $pos );
        if ( $ast->{FAIL} ) {
            return parse_fail( $tokens, $index );
        }
        $pos = $ast->{next};
    }
    if ( !$ast ) {
        $ast = parse_precedence_expression( $tokens, $pos, 0 );
        if ( $ast->{FAIL} ) {
            return parse_fail( $tokens, $index );
        }

        # mandatory semicolon or end-of-block or end-of-file
        $pos = $ast->{next};
        $pos = parse_optional_whitespace( $tokens, $pos )->{next};
        if (
            $tokens->[$pos][0] != END_TOKEN()    # not end of file
            && $tokens->[$pos][0] != SEMICOLON()
            && $tokens->[$pos][0] != CURLY_CLOSE()
          )
        {
            # Bareword found where operator expected (Missing operator before "a"?) at -e line 1, near "2 a"
            my $tok = $TOKEN_NAME{ $tokens->[$pos][0] };
            $tok = ucfirst( lc($tok) );
            die error_message( $tokens, $pos, $tok . ' found where operator expected (Missing operator before "' . $tokens->[$pos][1] . '"?)' );
        }
    }
    $pos = parse_optional_whitespace( $tokens, $pos )->{next};
    if ( $tokens->[$pos][0] == SEMICOLON() ) {
        $pos++;    # optional semicolon
    }
    $ast->{next} = $pos;
    return $ast;
}

sub token_as_string {
    my ( $type, $value, $attr ) = @_;
    return "" if !defined $value;
    $value = "newline" if $value eq "\n";
    $attr  = $TOKEN_NAME{ $attr // "" } // "";
    return "$TOKEN_NAME{$type}: \t'$value' \t $attr\n";
}

sub main {
    $Data::Dumper::Sortkeys = 1;
    $Data::Dumper::Indent   = 1;
    binmode( STDOUT, ":utf8" );
    my $perl_code = join( '', <DATA> );

    my $args = shift @ARGV;
    $perl_code = shift @ARGV if $args && $args eq '-e';

    my $tokens = tokenize($perl_code);

    ## # uncomment to see the token list
    ## for my $token (@$tokens) {
    ##     print token_as_string(@$token);
    ## }
    my $index = 0;
    while ( $tokens->[$index][0] != END_TOKEN() ) {
        $index = parse_optional_whitespace( $tokens, $index )->{next};
        last if $tokens->[$index][0] == END_TOKEN();
        my $ast = parse_statement( $tokens, $index, 0 );
        if ( !$ast->{FAIL} ) {
            print Data::Dumper::Dumper($ast);
            $index = $ast->{next};
        }
        else {
            print token_as_string( @{ $tokens->[$index] } );
            $index++;
        }
    }
}

main();

__DATA__
use strict;
use warnings;
my $var = 42;
2*3+5*6 or 0;
,,,;
(
!2   # a comment
 && not 4 + 1 );
'abc 123';
if (0) { 123; 456 }

{
	'abc 123 \\ \x \' \n ';
	"abc 123 \\ \x \' \n $x ";
}

do {
	'abc 123 \\ \x \' \n ';
	"abc 123 \\ \x \' \n $x ";
};
$a->[2] = $a->[3] + 4;
$a->[5]++;
--$a->[5];

$a = 5 ? [ 6 , 7 ] : func;

{}
{;; ; $a; }
{,}
$a = { a => 2 };
$a = {};
$a = {,,,};
$a->(123)[456]{aaa};
$a->(123)[456];
$a->(123);
$a->[123];
$a->{123};
$a[456]{aaa};
$a[456];
$$a;
$$a[1];
$$a->[1];
q! abd !;
q< abd >;
qq< abd [$v$a]  >;
(-123, -123.56,

=for testing pod

1E10 + -1E-10 );

	/ \n /;
	1 / 3;
m< abd [$v$a]  >;

qw( abc def \n &.= € );  

=encoding 123

# test a syntax error
# 2 + 3 5 + 8

if ($#var <=> 10.3E-2) {	# a comment
    print "The variable ${a} is greater than 10", "\n" or die("Error");
}

=pod 123
docs here
1+1
=cut
\$a
=2
;
{ , , a => 3 + 1, , c => 4 , , };
{ q => 123 };
print => 123;
$::a =~ /123/i;
__END__
123

