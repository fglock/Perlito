use strict;
use warnings;
use 5.034;
use utf8;
use Data::Dumper;

use constant {
    START      => 0,
    WHITESPACE => 1,
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
};

my %TokenName = (
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
    map { $_ => OPERATOR() }
      qw(
      == != <= >= < > <=>
      + * ** % ++ -- && || // ! ^ ~ ~~ & |
      >> <<
      =>
      **= += -= *= /= x= |= &= .= <<= >>= %= ||= &&= ^= //=
      |.= &.= ^.=
      )
);

# tokenize()
#
# known problems:
#   10E10     tokenizes to 10,E10
#   q=>=      tokenizes to q,=>,=	(but Perl also has problems with this)
#   q!=!=="=" tokenizes to q,!=,!=, ...
#
sub tokenize {
    my ($code) = @_;

    my $state = START();
    my @tokens;
    my $buffer     = '';
    my $quote_char = '';

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
            $buffer .= $char;
        }
        elsif ( $state == WHITESPACE() ) {
            if ( $char !~ /\s/ || $char eq "\n" ) {
                push @tokens, [ WHITESPACE(), $buffer ];
                $buffer = '';
                $state  = START();
                redo FSM;
            }
            else {
                $buffer .= $char;
            }
        }
        elsif ( $state == IDENTIFIER() ) {
            if ( $char !~ /[a-zA-Z0-9_]/ ) {
                push @tokens, [ IDENTIFIER(), $buffer ];
                $buffer = '';
                $state  = START();
                redo FSM;
            }
            else {
                $buffer .= $char;
            }
        }
        elsif ( $state == NUMBER() ) {
            if ( $char !~ /[0-9]/ ) {
                push @tokens, [ NUMBER(), $buffer ];
                $buffer = '';
                $state  = START();
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
                $buffer = '';
                $state  = START();
                redo FSM;
            }
        }
    }
    if ( $buffer ne '' ) {
        push @tokens, [ $state, $buffer ];
    }
    return \@tokens;
}

my %PRECEDENCE = (
    'or'  => 1,
    'xor' => 1,
    'and' => 2,
    'not' => 3,    # Unary negation

    ','  => 4,
    '=>' => 4,

    '='   => 5,
    '+='  => 5,
    '-='  => 5,
    '*='  => 5,
    '/='  => 5,
    '.='  => 5,
    'x='  => 5,
    '%='  => 5,
    '**=' => 5,

    '?' => 6,    # Ternary operator

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

    '!'  => 17,   # Unary negation
    '**' => 18,

    '++' => 19,
    '--' => 19,

    '->' => 21,

    '(' => 21,    # function call
    '{' => 21,    # hash element
    '[' => 21,    # array elemnt

    '$' => 22,
    '@' => 22,
    '%' => 22,
);

my %LIST = (
    ','  => 1,
    '=>' => 1,
);
my %PREFIX = (
    '!'   => 1,
    'not' => 1,
    '-'   => 1,
    '+'   => 1,
    '--'  => 1,
    '++'  => 1,
    '$'   => 1,
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
            $left_expr = { type => 'PREFIX_OP', value => [ $op_value, $expr ], next => $expr->{next} };
        }
    }
    else {
        $left_expr = parse_term( $tokens, $index );
    }
    if ( $left_expr->{FAIL} ) {
        return parse_fail( $tokens, $index );
    }

    while ( $pos < @$tokens ) {
        $pos = $left_expr->{next};
        $pos = parse_optional_whitespace( $tokens, $pos )->{next};
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
            next;
        }

        # Handle postfix operators
        if ( $POSTFIX{$op_value} ) {
            $left_expr = { type => 'POSTFIX_OP', value => [ $op_value, $left_expr ], next => $pos };
            next;
        }

        my $next_min_precedence = $ASSOC_RIGHT{$op_value} ? $precedence : $precedence + 1;
        my $right_expr          = parse_precedence_expression( $tokens, $pos, $next_min_precedence );
        if ( $right_expr->{FAIL} ) {
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
    while ( $pos < $#$tokens ) {
        last if !$tokens->[$pos];    # XXX autovivification
        if ( $tokens->[$pos][0] == WHITESPACE() ) {
            $pos++;
        }
        if ( $tokens->[$pos][0] == NEWLINE() ) {
            $pos++;
            last if !$tokens->[$pos];    # XXX autovivification
            if ( $tokens->[$pos][0] == EQUALS() ) {
                $pos++;
                if ( $tokens->[$pos][0] == IDENTIFIER() ) {

                    # documentation (pod):
                    # =for ... until end of paragraph
                    # =any_command ... until =cut or =end
                    # TODO
                    $pos++;
                    while ( $tokens->[$pos][0] != NEWLINE() ) {
                        $pos++;
                    }
                    redo;
                }
            }
            redo;
        }
        if ( $tokens->[$pos][0] == START_COMMENT() ) {
            $pos++;
            while ( $tokens->[$pos][0] != NEWLINE() ) {
                $pos++;
            }
            redo;
        }
        last;
    }
    return { type => 'WHITESPACE', index => $index, value => ' ', next => $pos };
}

sub parse_variable {
    my ( $tokens, $index ) = @_;
    my $pos = $index;
    if ( $tokens->[$pos][0] == SIGIL() ) {
        my $sigil = $tokens->[$pos][1];
        $pos++;    # $
        if ( $tokens->[$pos][0] == IDENTIFIER() ) {
            return {
                type  => 'VARIABLE',
                index => $index,
                value => [ $sigil, $tokens->[$pos][1], ],
                next  => $pos + 1
            };
        }
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

my %escape_sequence = qw/ a 7 b 8 e 27 f 12 n 10 r 13 t 9 /;

my %quote_pair = (
    '{' => '}',
    '(' => ')',
    '[' => ']',
    '<' => '>',
);

sub parse_single_quote_string {
    my ( $tokens, $index, $pos, $quote ) = @_;

    # 'abc'
    my $value;
    while ( $pos < @$tokens ) {
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
}

sub parse_double_quote_string {
    my ( $tokens, $index, $pos, $quote ) = @_;

    # "abc"
    my @ops;
    my $value = '';
    while ( $pos < @$tokens ) {
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
            if ( exists $escape_sequence{$c2} ) {
                $value .= chr( $escape_sequence{$c2} );
                $pos++;
                $pos++;
                next;
            }
        }
        if ( $type == SIGIL() && $tokens->[$pos][1] ne '%' ) {
            my $expr = parse_variable( $tokens, $pos );
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
}

sub parse_regex_string {
    my ( $tokens, $index, $pos, $quote ) = @_;

    # /abc/
    my @ops;
    my $value = '';
    while ( $pos < @$tokens ) {
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
            if ( exists $escape_sequence{$c2} ) {
                $value .= chr( $escape_sequence{$c2} );
                $pos++;
                $pos++;
                next;
            }
        }
        if ( $type == SIGIL() && $tokens->[$pos][1] ne '%' ) {
            my $expr = parse_variable( $tokens, $pos );
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
}

sub parse_string {
    my ( $tokens, $index ) = @_;
    my $pos = $index;
    if ( $tokens->[$pos][0] == STRING_DELIM() ) {
        my $quote = $tokens->[$pos][1];
        $pos++;    # "
        if ( $quote eq "'" ) {
            return parse_single_quote_string( $tokens, $index, $pos, $quote );
        }
        elsif ( $quote eq '"' ) {
            return parse_double_quote_string( $tokens, $index, $pos, $quote );
        }
        elsif ( $quote eq '`' ) {
            return parse_fail( $tokens, $index );
        }
    }
    return parse_fail( $tokens, $index );
}

sub parse_delim_expression {
    my ( $tokens, $index, $start, $stop ) = @_;
    my $pos       = $index;
    my $token_str = $TokenName{$start};
    if ( $tokens->[$pos][0] == $start ) {
        $pos = parse_optional_whitespace( $tokens, $pos + 1 )->{next};
        if ( $tokens->[$pos][0] == $stop ) {
            return { type => 'PAREN', value => [ $token_str, ], next => $pos + 1 };    # empty
        }
        my $expr = parse_precedence_expression( $tokens, $pos, 0 );
        if ( $expr->{FAIL} ) {
            return parse_fail( $tokens, $index );
        }
        $pos = $expr->{next};
        $pos = parse_optional_whitespace( $tokens, $pos )->{next};
        if ( $tokens->[$pos][0] == $stop ) {
            return { type => 'PAREN', value => [ $token_str, $expr ], next => $pos + 1 };
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
        while (1) {
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
    my $ast;
    if ( $type == NUMBER() || $type == DOT() ) {
        $ast = parse_number( $tokens, $index );
    }
    elsif ( $type == SIGIL() ) {
        $ast = parse_variable( $tokens, $index );
    }
    elsif ( $type == IDENTIFIER() ) {
        my $pos  = $index;
        my $stmt = $tokens->[$pos][1];
        if ( $stmt eq 'use' ) {
            $pos++;    # XXX special case just for testing!
            $pos = parse_optional_whitespace( $tokens, $pos )->{next};
            my $expr = parse_precedence_expression( $tokens, $pos, 0 );
            if ( $expr->{FAIL} ) {
                return parse_fail( $tokens, $index );
            }
            return { type => 'APPLY', stmt => $stmt, args => $expr, next => $expr->{next} };
        }
        if ( $stmt eq 'print' ) {
            $pos++;    # XXX special case just for testing!
            $pos = parse_optional_whitespace( $tokens, $pos )->{next};
            my $expr = parse_precedence_expression( $tokens, $pos, 0 );
            if ( $expr->{FAIL} ) {
                return parse_fail( $tokens, $index );
            }
            return { type => 'APPLY', stmt => $stmt, args => $expr, next => $expr->{next} };
        }
        if ( $stmt eq 'do' ) {
            $pos++;    # do BLOCK
            $pos = parse_optional_whitespace( $tokens, $pos )->{next};
            my $block = parse_statement_block( $tokens, $pos );
            if ( $block->{FAIL} ) {
                return parse_fail( $tokens, $index );
            }
            $ast = { type => 'DO_BLOCK', stmt => $stmt, block => $block, next => $block->{next} };
            return $ast;
        }
        if ( $stmt eq 'q' ) {
            $pos++;    # q!...!
            $pos = parse_optional_whitespace( $tokens, $pos )->{next};
            my $delim = $tokens->[$pos][1];
            if ( length($delim) > 1 ) {

                # tokenization fail; delimiter is ambiguous
            }
            else {
                if ( $quote_pair{$delim} ) { $delim = $quote_pair{$delim} }    # q< ... >
                $pos++;                                                        # 'abc'
                return parse_single_quote_string( $tokens, $index, $pos, $delim );
            }
        }
        if ( $stmt eq 'qq' ) {
            $pos++;                                                            # qq!...!
            $pos = parse_optional_whitespace( $tokens, $pos )->{next};
            my $delim = $tokens->[$pos][1];
            if ( length($delim) > 1 ) {

                # tokenization fail; delimiter is ambiguous
            }
            else {
                if ( $quote_pair{$delim} ) { $delim = $quote_pair{$delim} }    # q< ... >
                $pos++;                                                        # "abc"
                return parse_double_quote_string( $tokens, $index, $pos, $delim );
            }
        }
        if ( $stmt eq 'm' ) {
            $pos++;                                                            # /.../
            $pos = parse_optional_whitespace( $tokens, $pos )->{next};
            my $delim = $tokens->[$pos][1];
            if ( length($delim) > 1 ) {

                # tokenization fail; delimiter is ambiguous
            }
            else {
                if ( $quote_pair{$delim} ) { $delim = $quote_pair{$delim} }    # m< ... >
                $pos++;                                                        # m/abc/
                $ast = parse_regex_string( $tokens, $index, $pos, $delim );
                if ( $ast->{FAIL} ) {
                    return parse_fail( $tokens, $index );
                }

                # TODO parse regex modifiers
                return { type => 'REGEX', index => $index, args => $ast, next => $ast->{next} };
            }
        }
        if ( $stmt eq 'qw' ) {
            $pos++;    # qw/.../
            $pos = parse_optional_whitespace( $tokens, $pos )->{next};
            my $delim = $tokens->[$pos][1];
            if ( length($delim) > 1 ) {

                # tokenization fail; delimiter is ambiguous
            }
            else {
                if ( $quote_pair{$delim} ) { $delim = $quote_pair{$delim} }    # qw< ... >
                $pos++;
                $ast = parse_single_quote_string( $tokens, $index, $pos, $delim );
                if ( $ast->{FAIL} ) {
                    return parse_fail( $tokens, $index );
                }
                return { type => 'SPLIT', index => $index, args => [ ' ', $ast ], next => $ast->{next} };
            }
        }
        $ast = { type => 'BAREWORD', value => $tokens->[$index][1], next => $index + 1 };
    }
    elsif ( $type == STRING_DELIM() ) {
        $ast = parse_string( $tokens, $index );
    }
    elsif ( $type == PAREN_OPEN() ) {
        $ast = parse_delim_expression( $tokens, $index, PAREN_OPEN(), PAREN_CLOSE() );
    }
    elsif ( $type == SQUARE_OPEN() ) {
        $ast = parse_delim_expression( $tokens, $index, SQUARE_OPEN(), SQUARE_CLOSE() );
    }
    elsif ( $type == CURLY_OPEN() ) {
        $ast = parse_delim_expression( $tokens, $index, CURLY_OPEN(), CURLY_CLOSE() );
    }
    elsif ( $type == SLASH() ) {

        # /.../
        $ast = parse_regex_string( $tokens, $index, $index + 1, $tokens->[$index][1] );
        if ( $ast->{FAIL} ) {
            return parse_fail( $tokens, $index );
        }

        # TODO parse regex modifiers
        return { type => 'REGEX', index => $index, args => $ast, next => $ast->{next} };
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
    if ( $pos >= $#$tokens ) {
        return parse_fail( $tokens, $index );
    }
    my $ast;
    if ( $tokens->[$pos][0] == SEMICOLON() ) {
        $pos++;    # semicolon
        return { type => 'STATEMENT', stmt => 'empty_statement', next => $pos };
    }
    elsif ( $tokens->[$pos][0] == IDENTIFIER() ) {
        my $stmt = $tokens->[$pos][1];
        if ( $stmt eq 'if' || $stmt eq 'unless' || $stmt eq 'while' || $stmt eq 'until' ) {
            $pos++;
            $pos = parse_optional_whitespace( $tokens, $pos )->{next};
            my $expr = parse_delim_expression( $tokens, $pos, PAREN_OPEN(), PAREN_CLOSE() );
            if ( $expr->{FAIL} ) {
                return parse_fail( $tokens, $index );
            }
            $pos = $expr->{next};
            $pos = parse_optional_whitespace( $tokens, $pos )->{next};
            my $block = parse_statement_block( $tokens, $pos );
            if ( $block->{FAIL} ) {
                return parse_fail( $tokens, $index );
            }
            $ast = { type => 'STATEMENT', stmt => $stmt, condition => $expr, block => $block, next => $block->{next} };
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
            $tokens->[$pos][0]    # not end of file
            && $tokens->[$pos][0] != SEMICOLON()
            && $tokens->[$pos][0] != CURLY_CLOSE()
          )
        {
            my $tok = $TokenName{ $tokens->[$pos][0] };
            $tok = ucfirst( lc($tok) );
            die $tok . ' found where operator expected (Missing operator before "' . $tokens->[$pos][1] . '"?)';

            # Bareword found where operator expected (Missing operator before "a"?) at -e line 1, near "2 a"
        }
    }
    $pos = parse_optional_whitespace( $tokens, $pos )->{next};
    if ( $tokens->[$pos][0] && $tokens->[$pos][0] == SEMICOLON() ) {
        $pos++;    # optional semicolon
    }
    $ast->{next} = $pos;
    return $ast;
}

sub token_as_string {
    my ( $type, $value, $attr ) = @_;
    return "" if !defined $value;
    $value = "newline" if $value eq "\n";
    $attr  = $TokenName{ $attr // "" } // "";
    return "$TokenName{$type}: \t'$value' \t $attr\n";
}

sub main {
    binmode( STDOUT, ":utf8" );
    my $perl_code = join( '', <DATA> );
    my $tokens    = tokenize($perl_code);

    # for my $token (@$tokens) {
    #     print token_as_string(@$token);
    # }
    my $index = 0;
    while ( $index < @$tokens ) {
        $index = parse_optional_whitespace( $tokens, $index )->{next};
        last if $index >= @$tokens;
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
if ($#var <=> 10.3E-2) {	# a comment
    print "The variable is greater than 10\n";
}
2*3+5*6 or 0;
{ , , a => 3 + 1, , c => 4 , , };
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
{ q => 123 };
qq< abd [$v$a]  >;
(-123, -123.56,

=for testing pod

1E10 + -1E-10 );

	/ \n /;
	1 / 3;
m< abd [$v$a]  >;

qw( abc def \n &.= € );  

# test a syntax error
# 2 + 3 5 + 8

