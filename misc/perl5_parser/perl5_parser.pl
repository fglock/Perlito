use strict;
use warnings;
use 5.038;
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
    map { $_ => OPERATOR() }
      qw(
      == != <= >= < > <=> =
      + * ** / % ++ -- && || // ! ^ ~ ~~ & |
      >> <<
      -> =>
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
            elsif ( $char =~ /\d/ ) {
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
                push @tokens, [ WHITESPACE(), $buffer, 0 ];
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
            if ( $char !~ /\d/ ) {
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

    '**' => 17,
    '!'  => 18,    # Unary negation
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
);
my %POSTFIX = (
    '--' => 1,
    '++' => 1,
    ## ','   => 1,
);
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

        ## # Handle array indexing
        ## if ($op_value eq '[') {
        ##     $self->advance();
        ##     my $index_expr = $self->parse_expression();  # Parse the index expression
        ##     $self->advance() if $self->current_token()->[1] eq ']';  # Consume the closing bracket
        ##     $left_expr = SyntaxTreeNode->new('ARRAY_INDEX', $left_expr, $index_expr);
        ##     next;
        ## }

        # Handle ternary operator
        if ( $tokens->[$pos][0] == QUESTION() ) {
            $pos++;
            $pos = parse_optional_whitespace( $tokens, $pos )->{next};
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
            $pos++;
            $left_expr = { type => 'POSTFIX_OP', value => [ $op_value, $left_expr ], next => $pos };
            next;
        }

        last unless exists $PRECEDENCE{$op_value};
        my $precedence = $PRECEDENCE{$op_value};
        last if $precedence < $min_precedence;

        $pos++;
        $pos = parse_optional_whitespace( $tokens, $pos )->{next};

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
    if ( $tokens->[$pos][0] == MINUS() ) {
        $pos++;    # -
    }
    if ( $tokens->[$pos][0] == DOT() ) {
        $pos++;    # .
        if ( $tokens->[$pos][0] == NUMBER() ) {

            # .123
            $pos++;
        }
        else {
            return parse_fail( $tokens, $index );
        }
    }
    elsif ( $tokens->[$pos][0] == NUMBER() ) {
        $pos++;    # 123
        if ( $tokens->[$pos][0] == DOT() ) {

            # 123.
            $pos++;
            if ( $tokens->[$pos][0] == NUMBER() ) {

                # 123.456
                $pos++;
            }
        }
    }
    else {
        return parse_fail( $tokens, $index );
    }

    if ( $tokens->[$pos][0] == IDENTIFIER() && $tokens->[$pos][1] =~ /^e(\d*)$/i ) {
        if ($1) {

            # E10
            $pos++;
        }
        else {

            # 123E-10
            $pos++;

            if ( $tokens->[$pos][0] == MINUS() ) {

                # -
                $pos++;
            }
            if ( $tokens->[$pos][0] == NUMBER() ) {

                # 123
                $pos++;
            }
            else {
                return parse_fail( $tokens, $index );
            }
        }

        # return { NUMBER => 1, index => $index, value => [ map { $tokens->[$_] } $index .. $pos -1 ] };
        return { type => 'NUMBER', index => $index, value => join( '', map { $tokens->[$_][1] } $index .. $pos - 1 ), next => $pos };
    }
    else {
        return { type => 'NUMBER', index => $index, value => join( '', map { $tokens->[$_][1] } $index .. $pos - 1 ), next => $pos };
    }
}

my %escape_sequence = qw/ a 7 b 8 e 27 f 12 n 10 r 13 t 9 /;

sub parse_string {
    my ( $tokens, $index ) = @_;
    my $pos = $index;
    if ( $tokens->[$pos][0] == STRING_DELIM() ) {
        my $quote = $tokens->[$pos][1];
        $pos++;    # "
        if ( $quote eq "'" ) {

            # 'abc'
            my $value;
            while ( $pos < @$tokens ) {
                if ( $tokens->[$pos][0] == STRING_DELIM() && $quote eq $tokens->[$pos][1] ) {
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
        elsif ( $quote eq '"' ) {

            # "abc"
            my @ops;
            my $value = '';
            while ( $pos < @$tokens ) {
                my $type = $tokens->[$pos][0];
                if ( $type == STRING_DELIM() && $quote eq $tokens->[$pos][1] ) {
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
                if ( $type == SIGIL() ) {
                    my $expr = parse_variable( $tokens, $pos );
                    if ( $expr->{FAIL} ) {
                        return parse_fail( $tokens, $index );
                    }
                    push @ops, { type => 'STRING', index => $index, value => $value, next => $pos };
                    push @ops, $expr;
                    $value = '';
                    $pos   = $expr->{next};
                    next;
                }
                $value .= $tokens->[$pos][1];
                $pos++;
            }
        }
        elsif ( $quote eq '`' ) {
            return parse_fail( $tokens, $index );
        }
    }
    return parse_fail( $tokens, $index );
}

sub parse_delim_expression {
    my ( $tokens, $index, $start, $stop ) = @_;
    my $pos = $index;
    if ( $tokens->[$pos][0] == $start ) {
        $pos = parse_optional_whitespace( $tokens, $pos + 1 )->{next};
        my $expr = parse_precedence_expression( $tokens, $pos, 0 );
        if ( $expr->{FAIL} ) {
            return parse_fail( $tokens, $index );
        }
        $pos = $expr->{next};
        $pos = parse_optional_whitespace( $tokens, $pos )->{next};
        if ( $tokens->[$pos][0] == $stop ) {
            return { type => 'PAREN', value => [ $start, $expr ], next => $pos + 1 };
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
        if ( $stmt eq 'print' ) {

            # XXX special case just for testing!
            $pos++;
            $pos = parse_optional_whitespace( $tokens, $pos )->{next};
            my $expr = parse_precedence_expression( $tokens, $pos, 0 );
            if ( $expr->{FAIL} ) {
                return parse_fail( $tokens, $index );
            }
            return { type => 'APPLY', stmt => $stmt, args => $expr, next => $expr->{next} };
        }
        if ( $stmt eq 'do' ) {
	    # do BLOCK
            $pos++;
            $pos = parse_optional_whitespace( $tokens, $pos )->{next};
            my $block = parse_statement_block( $tokens, $pos );
            if ( $block->{FAIL} ) {
                return parse_fail( $tokens, $index );
            }
            $ast = { type => 'DO_BLOCK', stmt => $stmt, block => $block, next => $block->{next} };
            return $ast;
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
    if ( $tokens->[$pos][0] == IDENTIFIER() ) {
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
        $pos = $ast->{next};
    }
    $pos = parse_optional_whitespace( $tokens, $pos )->{next};
    if ( $tokens->[$pos][0] == SEMICOLON() ) {
        $pos++;    # optional semicolon
    }
    $ast->{next} = $pos;
    return $ast;
}

sub main {
    binmode( STDOUT, ":utf8" );
    my $perl_code = join( '', <DATA> );
    my $tokens    = tokenize($perl_code);
    ## for my $token (@$tokens) {
    ##     my ( $type, $value, $attr ) = @$token;
    ##     $value = "newline" if $value eq "\n";
    ##     $attr = $AttrName{$attr // ""} // "";
    ##     print "$TokenName{$type}: \t'$value' \t $attr\n";
    ## }
    my $index = 0;
    while ( $index < @$tokens ) {
        my $ast = parse_statement( $tokens, $index, 0 );
        if ( !$ast->{FAIL} ) {
            print Data::Dumper::Dumper($ast);
            $index = $ast->{next};
        }
        else {
            my ( $type, $value, $attr ) = @{ $tokens->[$index] };
            $value = "newline" if $value eq "\n";
            $attr  = $TokenName{ $attr // "" } // "";
            print "$TokenName{$type}: \t'$value' \t $attr\n";
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
qw( abc def \n &.= € );  
2*3+5*6 or 0;
1E10 + -1E-10;
5 ? [ 6 , 7 ] : func;
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
