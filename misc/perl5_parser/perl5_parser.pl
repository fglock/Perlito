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
#      [ END_TOKEN,  '' ],      # 3x end token
#      [ END_TOKEN,  '' ],
#      [ END_TOKEN,  '' ],
#      {                        # environment hash
#            filename => '-e',  # filename where the code comes from
#            here_doc => [],    # current "here documents" being processed
#      },
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
# TODO
#
#   namespaces
#
#   typeglobs
#       *HANDLE
#       *HANDLE{IO}
#
#   features
#       postderef feature
#       try-catch
#
#   keep track of declarations
#       sub
#       my, our, local, state
#
#   BEGIN
#
#   continue, else, elsif
#
#   attributes
#
#   subroutine
#       signatures
#       prototype
#       most builtin functions are missing
#
#   UTF8 parsing
#       delimiter pairs
#
#   warnings
#
#   indirect syntax (obsolete)
#
#   string interpolation
#   escape sequences like \x{263A}
#   octal numbers
#
#   tests
#

my %QUOTE_PAIR = (
    '{' => '}',
    '(' => ')',
    '[' => ']',
    '<' => '>',
);

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
    '**=' => 6,
    '+='  => 6,
    '*='  => 6,
    '&='  => 6,
    '&.=' => 6,
    '<<=' => 6,
    '&&=' => 6,
    '-='  => 6,
    '/='  => 6,
    '|='  => 6,
    '|.=' => 6,
    '>>=' => 6,
    '||=' => 6,
    '.='  => 6,
    '%='  => 6,
    '^='  => 6,
    '^.=' => 6,
    '//=' => 6,
    'x='  => 6,

    '?' => 7,    # Ternary operator

    '||' => 11,
    '&&' => 12,

    '=='  => 13,
    '!='  => 13,
    '<=>' => 13,
    'eq'  => 13,
    'ne'  => 13,
    'cmp' => 13,

    '<'  => 14,
    '>'  => 14,
    '<=' => 14,
    '>=' => 14,
    'lt' => 14,
    'gt' => 14,
    'le' => 14,
    'ge' => 14,

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

#
# Sub-Languages are code regions that don't follow the regular parsing rules
#

# parse_grammar( $tokens, $index,
#   { type => 'EXPR', opt => [ \&parse_optional_whitespace, \&PAREN_CLOSE ] },
# );
# parse_grammar( $tokens, $index,
#   { seq => [ \&parse_optional_whitespace, \&PAREN_CLOSE ] },
# );
#
sub parse_grammar {
    my ( $tokens, $index, $rule ) = @_;
    my @res;
    return $rule->( $tokens, $index ) if ref($rule) ne 'HASH';
    if ( $rule->{seq} ) {
        my $type = $rule->{type} // "";
        my $pos  = $index;
      SEQ:
        for my $rule ( @{ $rule->{seq} } ) {
            my $ast = parse_grammar( $tokens, $pos, $rule );
            if ( ref($ast) ne 'HASH' ) {
                if ( $ast < 0 ) {    # constant
                    return parse_fail() if $tokens->[$pos][0] != $ast;
                    $pos++;
                }
                else {
                    $pos = $ast;     # parse_optional_whitespace: always succeed
                }
                next SEQ;
            }
            return $ast if $ast->{FAIL};
            push @res, $ast;
            $pos = $ast->{next};
        }
        if ( @res == 1 && !$type ) {
            return { %{ $res[0] }, next => $pos };
        }
        return { type => $type, index => $index, value => \@res, next => $pos };
    }
    elsif ( $rule->{opt} ) {
        my $type = $rule->{type};
      OPT:
        for my $rule ( @{ $rule->{opt} } ) {
            my $ast = parse_grammar( $tokens, $index, $rule );
            if ( ref($ast) ne 'HASH' ) {
                my $pos = $index;
                if ( $ast < 0 ) {    # constant
                    next OPT if $tokens->[$index][0] != $ast;
                    return $pos + 1;
                }
                return $ast;         # parse_optional_whitespace: always succeed
            }
            next OPT                                                                       if $ast->{FAIL};
            return { type => $type, index => $index, value => $ast, next => $ast->{next} } if $type;
            return $ast;
        }
        return parse_fail();
    }
    elsif ( $rule->{before} ) {
        my ($rule) = @{ $rule->{before} };
        my $ast = parse_grammar( $tokens, $index, $rule );
        if ( ref($ast) ne 'HASH' ) {
            if ( $ast < 0 ) {    # constant
                return parse_fail() if $tokens->[$index][0] != $ast;
            }
            return $index;
        }
        return parse_fail() if $ast->{FAIL};
        return $index;
    }
    elsif ( $rule->{not_before} ) {
        my ($rule) = @{ $rule->{not_before} };
        my $ast = parse_grammar( $tokens, $index, $rule );
        if ( ref($ast) ne 'HASH' ) {
            if ( $ast < 0 ) {    # constant
                return $index if $tokens->[$index][0] != $ast;
            }
            return parse_fail();
        }
        return $index if $ast->{FAIL};
        return parse_fail();
    }
    die "malformed grammar";
}

sub parse_arg_list {
    my ( $tokens, $index ) = @_;
    return parse_precedence_expression( $tokens, $index, $LIST_OPERATOR_PRECEDENCE );
}

sub parse_single_arg {
    my ( $tokens, $index ) = @_;
    return parse_fail() if $tokens->[$index][0] == COMMA();
    return parse_precedence_expression( $tokens, $index, $PRECEDENCE{','} + 1 );
}

my $rule_block = { seq => [ { before => [ \&CURLY_OPEN ] }, \&parse_statement_block, ] };

sub meta_optional_parenthesis {
    my ($rule) = @_;
    return {
        opt => [
            {
                seq => [
                    \&PAREN_OPEN, \&parse_optional_whitespace, $rule,    # ( RULE )
                    \&parse_optional_whitespace, { opt => [ \&PAREN_CLOSE, \&error ] },
                ]
            },
            $rule,
        ],
    };
}

sub meta_parse_using {
    my ( $op_list, $grammar ) = @_;
    return map { $_ => $grammar } @$op_list;
}

my %SUB_LANGUAGE_HOOK = (

    # define placeholder parsers for Sub-Languages that we don't have yet
    meta_parse_using(
        [qw{ qq qx qr glob_string }],
        sub {    # qw//  1-argument raw string
            my ( $tokens, $index, $name, $ast ) = @_;
            my $pos = $index;
            $ast //= parse_raw_strings( $tokens, $pos, string_count => 1, name => $name );
            return $ast;
        }
    ),
    meta_parse_using(
        [qw{ tr y }],
        sub {    # s/abc/def/ig  3-argument raw string
            my ( $tokens, $index, $name, $ast ) = @_;
            my $pos = $index;
            $ast //= parse_raw_strings( $tokens, $pos, string_count => 3, name => $name );
            return $ast;
        }
    ),

    q => sub {    # 'abc'
        my ( $tokens, $pos, $name, $ast ) = @_;
        $ast //= parse_raw_strings( $tokens, $pos, string_count => 1, name => $name );
        my $str   = $ast->{value}{buffers}[0];
        my $delim = $ast->{value}{end_delim};
        $str =~ s{\\\Q$delim}{$delim}g;               # unescape
        $str =~ s{\\\\}{$delim}g if $delim ne '\\';
        $ast->{value} = $str;
        $ast->{type}  = 'STRING';
        return $ast;
    },
    m => sub {                                        # m/abc/ig
        my ( $tokens, $index, $name, $ast ) = @_;
        my $pos = $index;
        $ast //= parse_raw_strings( $tokens, $pos, string_count => 2, name => $name );
        return $ast;
    },
    s => sub {                                        # s/abc/def/ig
        my ( $tokens, $index, $name, $ast ) = @_;
        my $pos = $index;
        $ast //= parse_raw_strings( $tokens, $pos, string_count => 3, name => $name );
        return $ast;
    },
    qw => sub {                                       # qw/abc def/
        my ( $tokens, $index, $name, $ast ) = @_;
        my $pos = $index;
        $ast //= parse_raw_strings( $tokens, $pos, string_count => 1, name => $name );
        return $ast;
    },
    meta_parse_using(
        [qw{ time wantarray }],
        sub {
            my ( $tokens, $index, $name ) = @_;
            return parse_grammar(
                $tokens, $index,
                meta_optional_parenthesis(
                    {
                        type => "${name}_OP",
                        seq  => [],
                    },
                ),
            );
        }
    ),
    meta_parse_using(
        [
            'abs',        'alarm',    'chomp',         'chop',         'chr',            'chroot',
            'cos',        'defined',  'delete',        'eval',         'exists',         'exp',
            'getgrgid',   'getgrnam', 'gethostbyname', 'getnetbyname', 'getprotobyname', 'getprotobynumber',
            'getpwnam',   'getpwuid', 'hex',           'int',          'lc',             'lcfirst',
            'length',     'log',      'oct',           'ord',          'quotemeta',      'readlink',
            'readpipe',   'ref',      'rmdir',         'sethostent',   'setnetent',      'setprotoent',
            'setservent', 'sin',      'sqrt',          'uc',           'ucfirst',
        ],
        sub {
            my ( $tokens, $index, $name ) = @_;
            return parse_grammar(
                $tokens, $index,
                meta_optional_parenthesis(
                    {
                        type => "${name}_OP",
                        opt  => [ \&parse_single_arg, { seq => [] }, ]
                    },
                ),
            );
        }
    ),
    meta_parse_using(
        [qw{ map grep sort }],
        sub {
            my ( $tokens, $index, $name ) = @_;
            return parse_grammar(
                $tokens, $index,
                meta_optional_parenthesis(
                    {
                        type => "${name}_OP",
                        opt  => [
                            {    # BLOCK LIST
                                seq => [ $rule_block, \&parse_optional_whitespace, \&parse_arg_list, ]
                            },
                            \&parse_arg_list,
                            { seq => [] },
                        ],
                    }
                ),
            );
        }
    ),
    meta_parse_using(
        [qw{ my state our local }],    # XXX local has a different precedence
        sub {
            my ( $tokens, $index, $name ) = @_;
            return parse_grammar(
                $tokens, $index,
                {
                    type => "${name}_OP",
                    opt  => [
                        meta_optional_parenthesis(
                            {
                                opt => [
                                    \&parse_single_arg,    # XXX my EXPR
                                ],
                            }
                        ),
                    ],
                },
            );
        },
    ),
    meta_parse_using(
        [qw{ print printf say }],
        sub {
            my ( $tokens, $index, $name ) = @_;
            return parse_grammar(
                $tokens, $index,
                {
                    type => "${name}_OP",
                    opt  => [
                        meta_optional_parenthesis(
                            {
                                opt => [
                                    {    # print FILE LIST
                                        seq => [ \&parse_file_handle, \&parse_optional_whitespace, \&parse_arg_list ]
                                    },
                                    \&parse_arg_list,    # print LIST
                                    { seq => [] },       # print
                                ],
                            }
                        ),
                    ],
                },
            );
        },
    ),
    'do' => sub {
        my ( $tokens, $index, $name ) = @_;
        return parse_grammar(
            $tokens, $index,
            {
                type => "${name}_OP",
                seq  => [ \&parse_statement_block, ],
            },
        );
    },
    'eval' => sub {
        my ( $tokens, $index, $name ) = @_;
        return parse_grammar(
            $tokens, $index,
            {
                type => "${name}_OP",
                opt  => [
                    meta_optional_parenthesis(
                        {
                            opt => [
                                $rule_block,    # eval BLOCK
                                \&parse_one_arg, { seq => [] },
                            ],
                        }
                    ),
                ],
            },
        );
    },
    'sub' => sub {
        my ( $tokens, $index, $name ) = @_;
        return parse_grammar(
            $tokens, $index,
            {
                type => "${name}_OP",
                seq  => [ \&parse_statement_block, ],
            },
        );
    },
    'use' => sub {    # use module;
        my ( $tokens, $index, $name ) = @_;
        my $pos  = $index;
        my $expr = parse_precedence_expression( $tokens, $pos, $LIST_OPERATOR_PRECEDENCE );
        return parse_fail() if $expr->{FAIL};
        return { type => 'USE', value => { name => $name, args => $expr }, next => $expr->{next} };
    },
    'no' => sub {     # use module;
        my ( $tokens, $index, $name ) = @_;
        my $pos  = $index;
        my $expr = parse_precedence_expression( $tokens, $pos, $LIST_OPERATOR_PRECEDENCE );
        return parse_fail() if $expr->{FAIL};
        return { type => 'NO', value => { name => $name, args => $expr }, next => $expr->{next} };
    },
);

use constant {
    END_TOKEN  => -1,
    WHITESPACE => -2,
    KEYWORD    => -3,
    IDENTIFIER => -4,
    NUMBER     => -5,
    OPERATOR   => -9,
    STRING     => -11,

    STRING_DELIM  => -12,
    NEWLINE       => -13,
    START_COMMENT => -14,
    ESCAPE        => -15,
    MINUS         => -16,
    DOT           => -17,
    SIGIL         => -18,
    PAREN_OPEN    => -19,
    PAREN_CLOSE   => -20,
    QUESTION      => -21,
    COLON         => -22,
    COMMA         => -23,
    SQUARE_OPEN   => -24,
    SQUARE_CLOSE  => -25,
    CURLY_OPEN    => -26,
    CURLY_CLOSE   => -27,
    SEMICOLON     => -28,
    ARROW         => -29,
    EQUALS        => -30,
    SLASH         => -31,
    FAT_ARROW     => -32,
    DOUBLE_COLON  => -33,
    LESS_THAN     => -34,
    LESS_LESS     => -35,
    TILDE         => -36,
    START         => -37,
    PLUS          => -38,
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
    LESS_LESS()     => 'LESS_LESS',
    TILDE()         => 'TILDE',
    PLUS()          => 'PLUS',
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
    '<<' => LESS_LESS(),
    '~'  => TILDE(),
    '+'  => PLUS(),
    map { $_ => OPERATOR() }
      qw(
      == != <= >= > <=>
      =~ !~
      * ** % ++ -- && || // ! ^ ~~ & |
      >>

      **=   +=    *=    &=    &.=    <<=    &&=
      -=    /=    |=    |.=    >>=    ||=
      .=    %=    ^=    ^.=           //=   x=
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
    push @tokens, {    # environment hash
        filename => '',    # filename where the code comes from
        here_doc => [],    # current here documents being processed
    };
    return \@tokens;
}

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
    '**' => 1,

    '='   => 1,
    '**=' => 1,
    '+='  => 1,
    '*='  => 1,
    '&='  => 1,
    '&.=' => 1,
    '<<=' => 1,
    '&&=' => 1,
    '-='  => 1,
    '/='  => 1,
    '|='  => 1,
    '|.=' => 1,
    '>>=' => 1,
    '||=' => 1,
    '.='  => 1,
    '%='  => 1,
    '^='  => 1,
    '^.=' => 1,
    '//=' => 1,
    'x='  => 1,

    ','  => 1,
    '=>' => 1,
);    # /ASSOC_RIGHT
my %INFIX = (
    %ASSOC_RIGHT,

    'or'  => 1,
    'xor' => 1,
    'and' => 1,
    'not' => 1,    # Unary negation

    '?' => 1,      # Ternary operator

    '||' => 1,
    '&&' => 1,

    '=='  => 1,
    '!='  => 1,
    '<=>' => 1,
    'eq'  => 1,
    'ne'  => 1,
    'cmp' => 1,

    '<'  => 1,
    '>'  => 1,
    '<=' => 1,
    '>=' => 1,
    'lt' => 1,
    'gt' => 1,
    'le' => 1,
    'ge' => 1,

    '+' => 1,
    '-' => 1,
    '.' => 1,    # String concatenation

    '*' => 1,
    '/' => 1,
    '%' => 1,
    'x' => 1,    # String repetition

    '=~' => 1,
    '!~' => 1,

    '->' => 1,
    '('  => 1,    # function call
    '{'  => 1,    # hash element
    '['  => 1,    # array element
);                # /INFIX
my %NON_ASSOC_AUTO = (
    '++' => 1,
    '--' => 1,
);

sub error_message_quote {
    my ($to_quote) = @_;
    $to_quote =~ s/\n/\\n/g;
    if ( $to_quote !~ /"/ ) {
        return "\"$to_quote\"";
    }
    if ( $to_quote !~ /'/ ) {
        return "'$to_quote'";
    }
    return "<$to_quote>";
}

sub error {
    my ( $tokens, $index ) = @_;
    die error_message( $tokens, $index, "syntax error" );
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
        if ( $_ >= 0 ) {
            last if ref( $tokens->[$_] ) ne 'ARRAY';
            push @near, $tokens->[$_][1];
        }
    }

    ## my $col  = 1;    # retrieve column number
    ## for ( $first_token .. $index ) {
    ##     $col += length($tokens->[$_][1]);
    ## }
    return $message . ' at ' . $tokens->[-1]{filename} . ' line ' . $line . ', near ' . error_message_quote( join( '', @near ) ) . "\n";
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
        $pos = parse_optional_whitespace( $tokens, $pos + 1 );
        my $expr = parse_precedence_expression( $tokens, $pos, $PRECEDENCE{$op_value} );
        if ( $expr->{FAIL} ) {
            return parse_fail() if !$LIST{$op_value};

            # backtrack
            # Handle a lone comma and fat comma without any value
            return { type => 'LIST_OP', value => [$op_value], next => $pos };
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
                error( $tokens, $index );
            }
            $left_expr = { type => 'PREFIX_OP', value => { op => $op_value, arg => $expr }, next => $expr->{next} };
        }
        $pos = parse_optional_whitespace( $tokens, $left_expr->{next} );
        return $left_expr if $tokens->[$pos][0] == END_TOKEN();
    }
    else {
        $left_expr = parse_term( $tokens, $index );
        return parse_fail() if $left_expr->{FAIL};
        $pos = parse_optional_whitespace( $tokens, $left_expr->{next} );
        return $left_expr if $tokens->[$pos][0] == END_TOKEN();
    }

    $pos = $left_expr->{next};
    while ( $tokens->[$pos][0] != END_TOKEN() ) {
        $pos = parse_optional_whitespace( $tokens, $pos );
        return parse_fail() if $tokens->[$pos][0] == END_TOKEN();
        $op_value = $tokens->[$pos][1];
        my $type   = $tokens->[$pos][0];
        my $op_pos = $pos;

        last unless exists $INFIX{$op_value} || exists $POSTFIX{$op_value};
        my $precedence = $PRECEDENCE{$op_value};
        last if $precedence < $min_precedence;

        $pos = parse_optional_whitespace( $tokens, $pos + 1 );

        if ( $type == PAREN_OPEN() || $type == CURLY_OPEN() || $type == SQUARE_OPEN() ) {    # Handle postfix () [] {}
            my $right_expr = parse_term( $tokens, $op_pos );
            return parse_fail() if $right_expr->{FAIL};
            $left_expr = { type => 'APPLY_OR_DEREF', value => [ $left_expr, $right_expr ], next => $right_expr->{next} };
            $pos       = parse_optional_whitespace( $tokens, $left_expr->{next} );
            next;
        }
        if ( $type == ARROW() ) {                                                            # Handle ->method  ->method()  ->() ->[] ->{}
            $type = $tokens->[$pos][0];
            my $right_expr = parse_precedence_expression( $tokens, $pos, $PRECEDENCE{'->'} + 1 );
            return parse_fail() if $right_expr->{FAIL};
            if ( $type == PAREN_OPEN() || $type == CURLY_OPEN() || $type == SQUARE_OPEN() ) {    # Handle ->() ->[] ->{}
                $left_expr = { type => 'APPLY_OR_DEREF', value => [ $left_expr, $right_expr ], next => $right_expr->{next} };
            }
            else {
                $pos = parse_optional_whitespace( $tokens, $right_expr->{next} );
                if ( $tokens->[$pos][0] == PAREN_OPEN() ) {                                      # method call with arguments
                    my $args_expr = parse_term( $tokens, $pos );
                    return parse_fail() if $args_expr->{FAIL};
                    $left_expr = { type => 'METHOD_CALL', value => [ $left_expr, $right_expr, $args_expr ], next => $args_expr->{next} };
                }
                else {
                    $left_expr = { type => 'METHOD_CALL', value => [ $left_expr, $right_expr ], next => $right_expr->{next} };
                }
            }
            $pos = parse_optional_whitespace( $tokens, $left_expr->{next} );
            next;
        }    # /arrow
        if ( $type == QUESTION() ) {    # Handle ternary operator
            my $true_expr = parse_precedence_expression( $tokens, $pos, 0 );                    # Parse the true branch
            return parse_fail() if $true_expr->{FAIL};
            $pos = parse_optional_whitespace( $tokens, $true_expr->{next} );
            return parse_fail() if $tokens->[$pos][0] != COLON();
            $pos = parse_optional_whitespace( $tokens, $pos + 1 );
            my $false_expr = parse_precedence_expression( $tokens, $pos, $PRECEDENCE{'?'} );    # Parse the false branch
            $left_expr = { type => 'TERNARY_OP', value => [ '?', $left_expr, $true_expr, $false_expr ], next => $false_expr->{next} };
            $pos       = $left_expr->{next};
            next;
        }
        if ( $POSTFIX{$op_value} ) {                                                            # Handle postfix operators
            if (   $NON_ASSOC_AUTO{$op_value}
                && ( $left_expr->{type} eq 'POSTFIX_OP' || $left_expr->{type} eq 'PREFIX_OP' )
                && $NON_ASSOC_AUTO{ $left_expr->{value}{op} } )                                 # check for nonassoc syntax error
            {
                error( $tokens, $index );
            }
            $left_expr = { type => 'POSTFIX_OP', value => { op => $op_value, arg => $left_expr }, next => $pos };
            next;
        }

        my $next_min_precedence = $ASSOC_RIGHT{$op_value} ? $precedence : $precedence + 1;
        my $right_expr          = parse_precedence_expression( $tokens, $pos, $next_min_precedence );
        if ( $right_expr->{FAIL} ) {    # backtrack
            return parse_fail() if !$LIST{$op_value};    # Handle terminal comma and fat comma
            my @left = ($left_expr);
            if ( $left_expr->{type} eq 'LIST_OP' ) {
                @left = @{ $left_expr->{value} };
            }
            return { type => 'LIST_OP', value => [ @left, $op_value ], next => $pos };
        }
        if ( $LIST{$op_value} ) {    # Handle list separators (comma and fat comma)
            my @right = ($right_expr);
            if ( $right_expr->{type} eq 'LIST_OP' ) {
                @right = @{ $right_expr->{value} };
            }
            $left_expr = { type => 'LIST_OP', value => [ $left_expr, $op_value, @right ], next => $right_expr->{next} };
        }
        else {
            $left_expr = { type => 'BINARY_OP', value => [ $op_value, $left_expr, $right_expr ], next => $right_expr->{next} };
        }
        $pos = parse_optional_whitespace( $tokens, $left_expr->{next} );
    }
    return $left_expr;
}

sub parse_fail {
    return { FAIL => 1 };
}

sub parse_optional_whitespace {
    my ( $tokens, $index ) = @_;
    my $pos = $index;
  WS:
    while ( $tokens->[$pos][0] != END_TOKEN() ) {
        $pos++ if $tokens->[$pos][0] == WHITESPACE();
        if ( $tokens->[$pos][0] == NEWLINE() ) {
            $pos++;
            while ( my $here_doc = shift @{ $tokens->[-1]{here_doc} } ) {    # fetch "here doc" from the environment
                my $end_delim = $here_doc->{value}{buffers}[0];              # print <<"EOF";  print <<~"EOF";
                my $indented  = $here_doc->{value}{indented};
                my $quote     = $here_doc->{value}{start_delim};
                $here_doc->{value}{buffers}[0] = "";                         # initialize the string value
                my $indent_string;
              SEARCH_TERMINATOR: while (1) {
                    my $buffer = '';
                    while ( $tokens->[$pos][0] != NEWLINE() ) {
                        if ( $tokens->[$pos][0] == END_TOKEN() ) {
                            die error_message( $tokens, $index,
                                "Can't find string terminator " . error_message_quote($end_delim) . " anywhere before EOF" );
                        }
                        $buffer .= $tokens->[$pos][1];
                        $pos++;
                    }
                    $pos++;
                    last SEARCH_TERMINATOR                 if $buffer eq $end_delim;
                    ($indent_string) = $buffer =~ /^(\s+)/ if $indented && !defined($indent_string);
                    $buffer =~ s/^$indent_string// if $indented;
                    $here_doc->{value}{buffers}[0] .= $buffer . "\n";    # save the string value
                }
                my $processed_ast;
                if ( $quote eq "'" ) {                                   # process the quotes
                    $processed_ast = $SUB_LANGUAGE_HOOK{q}->( $tokens, $index, 'q', $here_doc );
                }
                elsif ( $quote eq '"' ) {
                    $processed_ast = $SUB_LANGUAGE_HOOK{qq}->( $tokens, $index, 'qq', $here_doc );
                }
                elsif ( $quote eq '`' ) {
                    $processed_ast = $SUB_LANGUAGE_HOOK{qx}->( $tokens, $index, 'qx', $here_doc );
                }
                $here_doc->{type}  = $processed_ast->{type}  if $processed_ast;
                $here_doc->{value} = $processed_ast->{value} if $processed_ast;
            }    # /here doc
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
            }    # /pod
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
            $pos = $#$tokens - 1;
        }
        last WS;
    }
    return $pos;
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
        return parse_fail();
    }
    return { type => 'COLON_BAREWORD', index => $index, value => \@tok, next => $pos };
}

sub parse_number {
    my ( $tokens, $index ) = @_;
    my $pos = $index;
    if ( $tokens->[$pos][0] == DOT() ) {
        $pos++;    # .
        return parse_fail() if $tokens->[$pos][0] != NUMBER();
        $pos++;    # .123
    }
    elsif ( $tokens->[$pos][0] == NUMBER() ) {
        $pos++;    # 123
        if ( $tokens->[$pos][0] == DOT() ) {
            $pos++;    # 123.
            if ( $tokens->[$pos][0] == NUMBER() ) {
                $pos++;    # 123.456
            }
        }
        elsif ( $tokens->[$pos][0] != IDENTIFIER() ) {    # no exponent
            return { type => 'INTEGER', index => $index, value => join( '', map { $tokens->[$_][1] } $index .. $pos - 1 ), next => $pos };
        }
    }
    else {
        return parse_fail();
    }
    if ( $tokens->[$pos][0] == IDENTIFIER() && $tokens->[$pos][1] =~ /^e([0-9]*)$/i ) {    # exponent
        if ($1) {
            $pos++;                                                                        # E10
        }
        else {
            $pos++;                                                                        # 123E-10
            $pos++              if $tokens->[$pos][0] == MINUS();                          # -
            return parse_fail() if $tokens->[$pos][0] != NUMBER();
            $pos++;                                                                        # 123
        }
    }
    return { type => 'NUMBER', index => $index, value => join( '', map { $tokens->[$_][1] } $index .. $pos - 1 ), next => $pos };
}

my %ESCAPE_SEQUENCE = qw/ a 7 b 8 e 27 f 12 n 10 r 13 t 9 /;

my %STATEMENT_MODIFIER = (
    'if'      => 1,
    'unless'  => 1,
    'while'   => 1,
    'until'   => 1,
    'for'     => 1,
    'foreach' => 1,
    'when'    => 1,
);
my %STATEMENT_COND_BLOCK = %STATEMENT_MODIFIER;

sub parse_raw_strings {
    my ( $tokens, $index, %args ) = @_;
    my $pos          = $index;
    my $string_count = $args{string_count} || die "need string_count";
    my $name         = $args{name}         || die "need name";

    # fetch the first string, and maybe the second:  s/FIRST/SECOND/g  s{FIRST}/bbb/ig
    my $redo = ( $string_count == 3 );
    my $ast  = parse_raw_string_with_delimiter( $tokens, $pos, $redo );    # use $redo flag to extract 2 strings
    $ast->{value}{name} = $name;
    return $ast if $string_count == 1;
    $pos = $ast->{next};

    if ( $string_count == 3 ) {                                            # fetch the second of 3 strings: s{aaa}{SECOND}ig
        my $delim = $ast->{value}{start_delim};                            #  / or {
        if ( $QUOTE_PAIR{$delim} ) {
            $pos = parse_optional_whitespace( $tokens, $pos );
            my $ast2 = parse_raw_string_with_delimiter( $tokens, $pos, 0 );
            push @{ $ast->{value}{buffers} }, @{ $ast2->{value}{buffers} };
            $ast->{next} = $ast2->{next};
            $pos = $ast->{next};
        }
    }

    # fetch the last string:    s/aaa/bbb/LAST
    my $modifier = "";
    if ( $tokens->[$pos][0] == IDENTIFIER() ) {
        $modifier = $tokens->[$pos][1];
        $ast->{next} = $pos + 1;
    }
    push @{ $ast->{value}{buffers} }, $modifier;
    return $ast;
}

# parse_raw_string_with_delimiter()
#
# extracts a string without any escaping
#
# use the $redo flag to extract 2 strings:  s///
#
# Note: this requires re-tokenizing this region of the code
# because of these known problems with the tokenizer:
#
#   q!=!=="=" tokenizes to ('q','!=','!=', ...)
#   q<>       tokenizes to ('<>')

sub parse_raw_string_with_delimiter {
    my ( $tokens, $index, $redo ) = @_;

    # quoted pairs can be embedded recursively:   q{ {x} }
    #
    # escape processing needs to happen AFTER the string is extracted
    #
    # variable interpolation needs to happen AFTER the string is extracted
    #   $ perl -e ' print "[[ @{[\"a\"]} ]] \n"; '
    #   [[ a ]]

    my $tok_pos     = $index;
    my $start_delim = '';
    my $end_delim   = '';
    my $state       = START();
    my $paren_level = 0;
    my $is_pair     = 0;
    my $buffer      = '';
    my $remain      = '';
    my @buffers;

    while ( $state != END_TOKEN() ) {
        if ( $tokens->[$tok_pos][0] == END_TOKEN() ) {
            die error_message( $tokens, $index, "Can't find string terminator " . error_message_quote($end_delim) . " anywhere before EOF" );
        }
      FSM:
        for my $char ( split //, $tokens->[$tok_pos][1] ) {
            if ( $state == START() ) {
                $start_delim = $char;
                $end_delim   = $start_delim;
                if ( $QUOTE_PAIR{$start_delim} ) {    # q< ... >
                    $is_pair   = 1;
                    $end_delim = $QUOTE_PAIR{$start_delim};
                }
                $state = STRING();
            }
            elsif ( $state == STRING() ) {
                if ( $is_pair && $char eq $start_delim ) {
                    $paren_level++;    # <
                }
                elsif ( $char eq $end_delim ) {
                    if ( $paren_level == 0 ) {
                        if ( $redo && !$is_pair ) {
                            push @buffers, $buffer;
                            $buffer = '';
                            $redo   = 0;
                            $state  = START();    # start again; one more string to fetch
                            redo FSM;
                        }
                        else {
                            $state = END_TOKEN();    # no more strings to fetch
                        }
                        next FSM;
                    }
                    $paren_level--;                  # >
                }
                elsif ( $char eq '\\' ) {
                    $state = ESCAPE();
                }
                $buffer .= $char;
            }
            elsif ( $state == ESCAPE() ) {
                $buffer .= $char;    #  handle \start_delim \end_delim
                $state = STRING();
            }
            elsif ( $state == END_TOKEN() ) {
                $remain .= $char;
            }
        }    # $char
        $tok_pos++;
    }
    push @buffers, $buffer;
    if ($remain) {

        # XXX what to do with $remain? put it back in $tokens
        $tokens->[ $tok_pos - 1 ][1] = $remain;    # put the remaining string back in the tokens list
    }
    return {
        type  => 'RAW_STRING',
        index => $index,
        next  => $tok_pos,
        value => { buffers => \@buffers, start_delim => $start_delim, end_delim => $end_delim },
    };
}

sub parse_for_expression {    #  (;  ;;  ;)  ()   (123;  (123)
    my ( $tokens, $index ) = @_;
    my $pos = $index;
    my $expr;
    error( $tokens, $index ) if $tokens->[$pos][1] ne ';' && $tokens->[$pos][1] ne '(';
    $pos = parse_optional_whitespace( $tokens, $pos + 1 );
    if ( $tokens->[$pos][1] ne ';' && $tokens->[$pos][1] ne ')' ) {
        $expr = parse_precedence_expression( $tokens, $pos, 0 );
        error( $tokens, $index ) if $expr->{FAIL};
        $pos = parse_optional_whitespace( $tokens, $expr->{next} );
        error( $tokens, $index ) if $tokens->[$pos][1] ne ';' && $tokens->[$pos][1] ne ')';
    }
    return { type => 'FOR_CONDITION', value => { delimiter => $tokens->[$pos][1], args => $expr }, next => $pos + 1 };
}

sub parse_delimited_expression {    #  )  123)
    my ( $tokens, $index, $start_delim, $end_delim ) = @_;
    my $pos = $index;
    my $expr;
    $pos = parse_optional_whitespace( $tokens, $pos );
    if ( $tokens->[$pos][1] ne $end_delim ) {
        $expr = parse_precedence_expression( $tokens, $pos, 0 );
        error( $tokens, $index ) if $expr->{FAIL};
        $pos = parse_optional_whitespace( $tokens, $expr->{next} );
        error( $tokens, $index ) if $tokens->[$pos][1] ne $end_delim;
    }
    return { type => 'PAREN', value => { delimiter => $start_delim, args => $expr }, next => $pos + 1 };
}

sub parse_statement_block {
    my ( $tokens, $index ) = @_;
    my $pos = $index;
    error( $tokens, $index ) if $tokens->[$pos][0] != CURLY_OPEN();
    $pos = parse_optional_whitespace( $tokens, $pos + 1 );
    my @expr;
    while (1) {
        error( $tokens, $index ) if $tokens->[$pos][0] == END_TOKEN();
        $pos = parse_optional_whitespace( $tokens, $pos );
        return { type => 'STATEMENT_BLOCK', value => \@expr, next => $pos + 1 } if $tokens->[$pos][0] == CURLY_CLOSE();
        my $expr = parse_statement( $tokens, $pos );
        error( $tokens, $index ) if $expr->{FAIL};
        push @expr, $expr;
        $pos = $expr->{next};
    }
}

sub parse_file_handle {

    # TODO implement the remaining rules
    #
    # is file handle:
    #
    #   print $f LIST ;  print {$f} LIST ;  print STDOUT LIST ;  print {STDOUT} LIST
    #   print STDOUT;
    #   print $f +3;        # plus is close to the second argument
    #   print $f or die;    # `or` is not an argument to print
    #   print STDOUT (10);
    #   print $fh(10);      # not a function call
    #
    # is NOT file handle:
    #
    #   print chr;          # a known function name
    #   print chr 100       # a known function name
    #   print $f;           # can't tell if this is a filehandle at compile-time
    #   print $f+3;         # infix plus
    #   print $f + 3;       # infix plus
    #   print STDOUT(10);   # function call
    #

    my ( $tokens, $index ) = @_;
    my $pos = $index;
    my $ast;
    if ( $tokens->[$pos][0] == IDENTIFIER() ) {

        # TODO check that the bareword is not a builtin like `chr`

        $ast = { type => 'BAREWORD', value => $tokens->[$pos][1], next => $pos + 1 };
    }
    elsif ( $tokens->[$pos][0] == SIGIL() && $tokens->[$pos][1] eq '$' ) {
        $ast = parse_precedence_expression( $tokens, $pos, $PRECEDENCE{'$'} );
    }
    elsif ( $tokens->[$pos][0] == CURLY_OPEN() ) {
        $ast = parse_delimited_expression( $tokens, $pos + 1, '{', '}' );
    }
    return parse_fail() if !$ast || $ast->{FAIL};
    $pos = $ast->{next};
    $ast = { type => 'FILE_HANDLE', value => $ast, next => $pos };

    # we have a symbol; now check the syntax that follows the filehandle

    # TODO check for terminators: END_TOKEN, SEMICOLON, PAREN_CLOSE, CURLY_CLOSE, SQUARE_CLOSE

    return parse_fail() if $tokens->[$pos][0] != WHITESPACE() && $tokens->[$pos][0] != NEWLINE();    # must be followed by space
    $pos = parse_optional_whitespace( $tokens, $pos + 1 );
    my $tok = $tokens->[$pos][1];
    return parse_fail() if $INFIX{$tok} && $PRECEDENCE{$tok} > $LIST_OPERATOR_PRECEDENCE; # must not be followed by a higher precedence infix operator
    return $ast;
}

sub parse_term {
    my ( $tokens, $index ) = @_;
    my $pos  = $index;
    my $type = $tokens->[$pos][0];
    my $ast;
    if ( $type == NUMBER() || $type == DOT() ) {
        return parse_number( $tokens, $index );
    }
    elsif ( $type == IDENTIFIER() ) {

        if ( $tokens->[ $pos + 1 ][0] == DOUBLE_COLON() ) {
            return parse_colon_bareword( $tokens, $index );    # TODO parse special cases like CORE::print
        }

        my $stmt = $tokens->[$pos][1];
        $pos = parse_optional_whitespace( $tokens, $pos + 1 );
        if ( $tokens->[$pos][0] == FAT_ARROW() ) {             # bareword
            return { type => 'STRING', value => $tokens->[$index][1], next => $index + 1 };
        }
        if ( exists $SUB_LANGUAGE_HOOK{$stmt} ) {              # built-in functions requiring special parsing
            return $SUB_LANGUAGE_HOOK{$stmt}->( $tokens, $pos, $stmt );
        }
        return { type => 'BAREWORD', value => $tokens->[$index][1], next => $index + 1 };
    }
    elsif ( $type == STRING_DELIM() ) {
        my $quote = $tokens->[$index][1];
        if ( $quote eq "'" ) {
            return $SUB_LANGUAGE_HOOK{q}->( $tokens, $index, 'q' );
        }
        elsif ( $quote eq '"' ) {
            return $SUB_LANGUAGE_HOOK{qq}->( $tokens, $index, 'qq' );
        }
        elsif ( $quote eq '`' ) {
            return $SUB_LANGUAGE_HOOK{qx}->( $tokens, $index, 'qx' );
        }
    }
    elsif ( $type == PAREN_OPEN() ) {
        return parse_delimited_expression( $tokens, $index + 1, '(', ')' );
    }
    elsif ( $type == SQUARE_OPEN() ) {
        return parse_delimited_expression( $tokens, $index + 1, '[', ']' );
    }
    elsif ( $type == CURLY_OPEN() ) {
        return parse_delimited_expression( $tokens, $index + 1, '{', '}' );
    }
    elsif ( $type == LESS_THAN() ) {
        return $SUB_LANGUAGE_HOOK{glob_string}->( $tokens, $index, 'glob_string' );    # <...>
    }
    elsif ( $type == SLASH() ) {                                                       # /.../
        return $SUB_LANGUAGE_HOOK{m}->( $tokens, $index, 'm' );
    }
    elsif ( $type == LESS_LESS() ) {                                                   # here doc:  <<   <<~
        $pos++;
        my $indented = 0;
        if ( $tokens->[$pos][0] == TILDE() ) {
            $pos++;
            $indented = 1;
        }
        $pos = parse_optional_whitespace( $tokens, $pos );
        if ( $tokens->[$pos][0] == IDENTIFIER() ) {    # bareword
            die error_message( $tokens, $pos, 'Use of bare << to mean <<"" is forbidden' );
        }
        if ( $tokens->[$pos][0] == ESCAPE() ) {        # \IDENTIFIER  is the same as 'IDENTIFIER'
            if ( $tokens->[ $pos + 1 ][0] == IDENTIFIER() ) {    # bareword
                $pos++;
                $ast = {
                    type  => 'RAW_STRING',
                    index => $index,
                    next  => $pos + 1,
                    value => { buffers => [ $tokens->[$pos][1] ], start_delim => "'", end_delim => "'" },
                };
            }
        }
        elsif ( $tokens->[$pos][0] == STRING_DELIM() ) {
            $ast = parse_raw_string_with_delimiter( $tokens, $pos, 0 );
        }
        return parse_fail() if !$ast || $ast->{FAIL};
        $ast->{value}{indented} = $indented;
        push @{ $tokens->[-1]{here_doc} }, $ast;
        return $ast;
    }
    elsif ( $type = DOUBLE_COLON() ) {
        return parse_colon_bareword( $tokens, $index );
    }
    return parse_fail();
}

sub parse_statement {
    my ( $tokens, $index ) = @_;
    my $pos = $index;
    $pos = parse_optional_whitespace( $tokens, $pos );
    return parse_fail() if $tokens->[$pos][0] == END_TOKEN();

    my $ast;
    if ( $tokens->[$pos][0] == SEMICOLON() ) {
        return { type => 'STATEMENT', value => { stmt => 'empty_statement' }, next => $pos + 1 };
    }
    elsif ( $tokens->[$pos][0] == IDENTIFIER() ) {
        my $stmt = $tokens->[$pos][1];

        my $pos1 = parse_optional_whitespace( $tokens, $pos + 1 );
        if ( $tokens->[$pos1][0] == COLON() ) {    # LABEL:
            return { type => 'LABEL', index => $index, value => $stmt, next => $pos1 + 1 };
        }

        if ( $STATEMENT_COND_BLOCK{$stmt} ) {
            $pos = parse_optional_whitespace( $tokens, $pos + 1 );
            my $expr = parse_for_expression( $tokens, $pos );
            if ( $expr->{value}{delimiter} eq ';' ) {
                error( $tokens, $index ) if $stmt ne 'for' && $stmt ne 'foreach';
                my @expr = ($expr);
                $expr = parse_for_expression( $tokens, $expr->{next} - 1 );
                push @expr, $expr;
                $expr = parse_for_expression( $tokens, $expr->{next} - 1 );
                push @expr, $expr;
                $expr = { type => 'THREE_ARG_FOR', value => \@expr, next => $expr->{next} };
            }
            error( $tokens, $index ) if $expr->{FAIL};
            $pos = parse_optional_whitespace( $tokens, $expr->{next} );
            my $block = parse_statement_block( $tokens, $pos );

            # TODO if/unless .. else/elsif
            # TODO continue

            $ast = { type => 'STATEMENT', value => { stmt => $stmt, condition => $expr, block => $block }, next => $block->{next} };
            $pos = $ast->{next};
        }
    }
    elsif ( $tokens->[$pos][0] == CURLY_OPEN() ) {

        # BEGIN, END, INIT, CHECK, UNITCHECK

        $ast = parse_statement_block( $tokens, $pos );

        # TODO continue

        $pos = $ast->{next};
    }
    if ( !$ast ) {
        $ast = parse_precedence_expression( $tokens, $pos, 0 );
        error( $tokens, $index ) if $ast->{FAIL};

        $pos = parse_optional_whitespace( $tokens, $ast->{next} );
        if ( $tokens->[$pos][0] == IDENTIFIER() ) {    # statement modifier
            my $stmt = $tokens->[$pos][1];
            if ( $STATEMENT_MODIFIER{$stmt} ) {
                $pos = parse_optional_whitespace( $tokens, $pos + 1 );
                my $cond_ast = parse_precedence_expression( $tokens, $pos, 0 );
                error( $tokens, $index ) if $cond_ast->{FAIL};
                $ast = {
                    type  => 'STATEMENT_MODIFIER',
                    index => $ast->{index},
                    value => { modifier => $stmt, args => $ast, cond => $cond_ast },
                    next  => $cond_ast->{next}
                };
                $pos = parse_optional_whitespace( $tokens, $ast->{next} );
            }
        }

        if (    # mandatory semicolon or end-of-block or end-of-file
            $tokens->[$pos][0] != END_TOKEN()    # not end of file
            && $tokens->[$pos][0] != SEMICOLON()
            && $tokens->[$pos][0] != CURLY_CLOSE()
          )
        {                                        # Bareword found where operator expected (Missing operator before "a"?) at -e line 1, near "2 a"
            my $tok = $TOKEN_NAME{ $tokens->[$pos][0] };
            $tok = ucfirst( lc($tok) );
            die error_message( $tokens, $pos, $tok . ' found where operator expected (Missing operator before "' . $tokens->[$pos][1] . '"?)' );
        }
    }
    $pos = parse_optional_whitespace( $tokens, $pos );
    if ( $tokens->[$pos][0] == SEMICOLON() ) {
        $pos++;                                  # optional semicolon
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
    $perl_code = shift(@ARGV) . "\n" if $args && $args eq '-e';

    my $tokens = tokenize($perl_code);
    $tokens->[-1]{filename} = '-e';    # initialize environment hash

    ## # uncomment to see the token list
    ## for my $token (@$tokens) {
    ##     print token_as_string(@$token);
    ## }
    my $index = 0;
    while ( $tokens->[$index][0] != END_TOKEN() ) {
        $index = parse_optional_whitespace( $tokens, $index );
        last if $tokens->[$index][0] == END_TOKEN();
        my $ast = parse_statement( $tokens, $index, 0 );
        if ( !$ast->{FAIL} ) {
            $index = parse_optional_whitespace( $tokens, $ast->{next} );
            print Data::Dumper::Dumper($ast);
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

print <<"EOT", <<~'AAA', "abc";
  xxx
EOT
    ooo
      oOo
AAA

$a = <<'EOT';
EOT

$a = <<\EOT;
 single quoted
EOT

$a->$b;
$a->b(123) if $b;

LABEL: OTHER_LABEL: foreach ( 1;2;3 ) { $a }

__END__
123

