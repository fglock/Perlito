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
#       __PACKAGE__
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
#   attributes
#
#   subroutine
#       more signatures and prototypes
#       more builtin functions
#       __SUB__
#
#   UTF8 parsing
#       delimiter pairs
#
#   warnings
#
#   indirect syntax (obsolete)
#
#   string interpolation (sublanguages)
#   escape sequences like \x{263A}
#   octal numbers
#
#   test block disambiguation
#
#       sub XX {print 123} {XX}();  # prints 123
#
#       sub XX {print 123} +{XX}(); # syntax error
#
#       {ZZ}();                     # no error: block followed by list
#
#       return {ZZ}();              # syntax error
#
#   tests
#

my %QUOTE_PAIR = (
    '{' => '}',
    '(' => ')',
    '[' => ']',
    '<' => '>',
);
my %PRECEDENCE = (
    ( map { $_ => 1 } qw(or xor) ),
    ( map { $_ => 2 } qw(and) ),
    ( map { $_ => 3 } qw(not) ),
    ( map { $_ => 5 } ',', '=>' ),
    ( map { $_ => 6 } qw(= **= += *= &= &.= <<= &&= -= /= |= |.= >>= ||= .= %= ^= ^.= //= x=) ),
    ( map { $_ => 7 } qw(?) ),
    ( map { $_ => 8 } qw(.. ...) ),
    ( map { $_ => 9 } qw(|| ^^ //) ),
    ( map { $_ => 10 } qw(&&) ),
    ( map { $_ => 11 } qw(== != <=> eq ne cmp) ),
    ( map { $_ => 12 } qw(< > <= >= lt gt le ge) ),
    ( map { $_ => 13 } qw(+ - .) ),
    ( map { $_ => 14 } qw(* / % x) ),
    ( map { $_ => 15 } qw(=~ !~) ),
    ( map { $_ => 16 } qw(! \\) ),
    ( map { $_ => 17 } qw(**) ),
    ( map { $_ => 18 } qw(++ --) ),
    ( map { $_ => 19 } qw(-> { [ ), '(' ),
    ( map { $_ => 20 } qw($ @ %),   '$#' ),
);
my %LIST    = map { $_ => 1 } ',', '=>';
my %PREFIX  = map { $_ => 1 } qw( ! \\ not - + -- ++ $ @ % * & ), '$#';
my %POSTFIX = map { $_ => 1 } qw( -- ++ );

# default associativity is LEFT
my %ASSOC_RIGHT = map { $_ => 1 } qw(** = **= += *= &= &.= <<= &&= -= /= |= |.= >>= ||= .= %= ^= ^.= //= x= =>), ',';
my %INFIX       = (
    %ASSOC_RIGHT, map { $_ => 1 } qw(or xor and ? || && == != <=> eq ne cmp < > <= >= lt gt le ge + - . * / // % x =~ !~ -> { [ ),
    '(', '..', '...'
);
my %NON_ASSOC_AUTO = map { $_ => 1 } qw( -- ++ );

my %FORBIDDEN_CALL = map { $_ => 1 } qw($ @ % * ), '$#';    # $x() is forbidden

my %ESCAPE_SEQUENCE = qw/ a 7 b 8 e 27 f 12 n 10 r 13 t 9 /;

my %STATEMENT_MODIFIER   = map { $_ => 1 } qw/ if unless while until for foreach when /;
my %STATEMENT_COND_BLOCK = %STATEMENT_MODIFIER;
my %RESERVED_WORDS       = ( %STATEMENT_MODIFIER, map { $_ => 1 } qw/ else elsif continue / );
my %LIST_TERMINATOR      = ( %STATEMENT_MODIFIER, map { $_ => 1 } qw/ or xor and not ; / );

#
# Tokenizer/Lexer
#

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

my %TOKEN_NAME = (    # this is used to produce error messages
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

my %OPERATORS = (    # the tokenizer uses this to map strings to token numbers
    ','     => COMMA(),
    '#'     => START_COMMENT(),
    "'"     => STRING_DELIM(),
    '"'     => STRING_DELIM(),
    '`'     => STRING_DELIM(),
    '\\'    => ESCAPE(),
    '-'     => MINUS(),
    '.'     => DOT(),
    '$'     => SIGIL(),
    '$#'    => SIGIL(),
    '@'     => SIGIL(),
    '%'     => SIGIL(),
    '*'     => SIGIL(),
    '&'     => SIGIL(),
    '('     => PAREN_OPEN(),
    ')'     => PAREN_CLOSE(),
    '?'     => QUESTION(),
    ':'     => COLON(),
    '['     => SQUARE_OPEN(),
    ']'     => SQUARE_CLOSE(),
    '{'     => CURLY_OPEN(),
    '}'     => CURLY_CLOSE(),
    ';'     => SEMICOLON(),
    '->'    => ARROW(),
    '='     => EQUALS(),
    '/'     => SLASH(),
    '=>'    => FAT_ARROW(),
    '::'    => DOUBLE_COLON(),
    '<'     => LESS_THAN(),
    '<<'    => LESS_LESS(),
    '~'     => TILDE(),
    '+'     => PLUS(),
    chr(10) => NEWLINE(),
    chr(13) => NEWLINE(),
    map { $_ => OPERATOR() }
      qw( == != <= >= > <=> =~ !~ .. ...
      ** % ++ -- && || ^^ // ! ^ ~~ | >>
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
my %TOK_SPACE             = map { $_ => WHITESPACE() } " ", "\t", "\f", chr(11);
my %TOK_IDENTIFIER        = map { $_ => IDENTIFIER() } "a" .. "z", "A" .. "Z", "_";
my %TOK_IDENTIFIER_NUMBER = map { $_ => IDENTIFIER() } "a" .. "z", "A" .. "Z", "_", "0" .. "9";
my %TOK_NUMBER            = map { $_ => NUMBER() } "0" .. "9";
my %TOK_OPERATOR          = map { $_ => OPERATOR() } keys %OPERATORS;
my %NEXT_STATE            = ( %TOK_SPACE, %TOK_IDENTIFIER, %TOK_NUMBER, %TOK_OPERATOR );

sub tokenize {
    my ($code) = @_;
    my $state = START();
    my @tokens;
    my $buffer;
  FSM:
    for my $char ( split //, $code ) {
        if ( $state == WHITESPACE() ) {
            if ( exists $TOK_SPACE{$char} ) {
                $buffer .= $char;
            }
            else {
                push @tokens, [ WHITESPACE(), $buffer ];
                if ( $state = $NEXT_STATE{$char} ) {
                    $buffer = $char;
                }
                else {
                    $state = START();
                    push @tokens, [ STRING(), $char ];
                }
            }
        }
        elsif ( $state == IDENTIFIER() ) {
            if ( exists $TOK_IDENTIFIER_NUMBER{$char} ) {
                $buffer .= $char;
            }
            else {
                push @tokens, [ IDENTIFIER(), $buffer ];
                if ( $state = $NEXT_STATE{$char} ) {
                    $buffer = $char;
                }
                else {
                    $state = START();
                    push @tokens, [ STRING(), $char ];
                }
            }
        }
        elsif ( $state == NUMBER() ) {
            if ( exists $TOK_NUMBER{$char} ) {
                $buffer .= $char;
            }
            else {
                push @tokens, [ NUMBER(), $buffer ];
                if ( $state = $NEXT_STATE{$char} ) {
                    $buffer = $char;
                }
                else {
                    $state = START();
                    push @tokens, [ STRING(), $char ];
                }
            }
        }
        elsif ( $state == OPERATOR() ) {
            if ( exists $OPERATORS{"$buffer$char"} ) {
                $buffer .= $char;
            }
            else {
                push @tokens, [ $OPERATORS{$buffer}, $buffer ];

                if ( $state = $NEXT_STATE{$char} ) {
                    $buffer = $char;
                }
                else {
                    $state = START();
                    push @tokens, [ STRING(), $char ];
                }
            }
        }
        elsif ( $state == START() ) {
            if ( $state = $NEXT_STATE{$char} ) {
                $buffer = $char;
            }
            else {
                $state = START();
                push @tokens, [ STRING(), $char ];
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
        filename     => '',        # filename where the code comes from
        here_doc     => [],        # current here documents being processed
        package_name => 'main',    # stack with current __PACKAGE__
    };
    return \@tokens;
}

#
# Runtime and scope data
#

sub env_set_current_filename {
    my ( $tokens, $filename ) = @_;
    $tokens->[-1]{filename} = $filename;
}

sub env_get_current_filename {
    my ($tokens) = @_;
    return $tokens->[-1]{filename};
}

sub env_set_subroutine {
    my ( $tokens, $name, $data ) = @_;
    $tokens->[-1]{declared_sub}{$name} = $data;
}

sub env_get_subroutine {
    my ( $tokens, $name ) = @_;
    return $tokens->[-1]{declared_sub}{$name};
}

sub env_set_package {
    my ( $tokens, $data ) = @_;
    $tokens->[-1]{package} = $data;
}

sub env_get_package {
    my ($tokens) = @_;
    return $tokens->[-1]{package};
}

#
# Sub-Languages are code regions that don't follow the regular parsing rules
#

# meta_grammar( $tokens, $index,
#   { type => 'EXPR', opt => [ \&parse_optional_whitespace, \&PAREN_CLOSE ] },
# );
# meta_grammar( $tokens, $index,
#   { seq => [ \&parse_optional_whitespace, \&PAREN_CLOSE ] },
# );
#
sub meta_grammar {
    my ( $tokens, $index, $rule ) = @_;
    my @res;
    return $rule->( $tokens, $index ) if ref($rule) ne 'HASH';
    if ( $rule->{seq} ) {
        my $type = $rule->{type} // "";
        my $pos  = $index;
      SEQ:
        for my $rule ( @{ $rule->{seq} } ) {
            my $ast = meta_grammar( $tokens, $pos, $rule );
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
            my $ast = meta_grammar( $tokens, $index, $rule );
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
        my $ast = meta_grammar( $tokens, $index, $rule );
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
        my $ast = meta_grammar( $tokens, $index, $rule );
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

# Argument types --------

my %START_WHITESPACE = map { $_ => 1 } WHITESPACE(), NEWLINE(), START_COMMENT();

sub parse_arg_list {
    my ( $tokens, $index, %opt ) = @_;
    my $signature = $opt{signature};    # $ $$ $$$ $@ &@      https://perldoc.perl.org/perlsub#Prototypes
    my $first     = $opt{first};        # we've already parsed something
    my $sub_name  = $opt{sub_name};     # used for error messages

    my $pos = $index;
    my $ast;
    my @expr;
    push @expr, $first if $first;
    my $seen_comma = 1;
    my $optional   = 0;

    if ( defined($signature) ) {
        return parse_fail() if @expr && $signature eq '';    # signature says zero args, but we've already consumed args
        if ( substr( $signature, 0, 1 ) eq '&' ) {           # &@
            parse_fail() if @expr;
            my $block = parse_statement_block( $tokens, $pos );
            push @expr, $block;
            $pos       = $block->{next};
            $signature = substr( $signature, 1 );
            if ( substr( $signature, 0, 1 ) eq ';' ) {       # ;$
                $signature = substr( $signature, 1 );
                $optional  = 1;
            }
        }
    }

  LIST:
    while (1) {
        last LIST if defined($signature) && $signature eq '';    # no more arguments
        $pos++    if $tokens->[$pos][0] == WHITESPACE();
        $pos = parse_optional_whitespace( $tokens, $pos )
          if $START_WHITESPACE{ $tokens->[$pos][0] };
        last LIST if $tokens->[$pos][0] == END_TOKEN();
        if ( $tokens->[$pos][0] == COMMA() || $tokens->[$pos][0] == FAT_ARROW() ) {
            $seen_comma = 1;
            $pos++;
            next LIST;
        }
        last LIST if $LIST_TERMINATOR{ $tokens->[$pos][1] };
        last LIST if !$seen_comma;
        $ast = parse_precedence_expression( $tokens, $pos, $PRECEDENCE{','} + 1 );
        last LIST if $ast->{FAIL};

        if ( defined($signature) ) {
            my $sig = substr( $signature, 0, 1 );

            # TODO - improve checking for argument type  $ @ *
            return parse_fail() if $ast->{type} eq 'PREFIX_OP' && $ast->{value}{op} ne $sig;
            $signature = substr( $signature, 1 );
            if ( substr( $signature, 0, 1 ) eq ';' ) {    # ;$
                $signature = substr( $signature, 1 );
                $optional  = 1;
            }
        }

        $pos = $ast->{next};
        push @expr, $ast;
        $seen_comma = 0;
    }
    if ( !$optional && defined($signature) && $signature ne '' ) {    # the signature was not consumed
        die error_message( $tokens, $pos, "Not enough arguments" . ( $sub_name ? " for $sub_name" : "" ) );
    }

    return { %{ $expr[0] }, next => $pos } if @expr == 1;
    return { type => 'LIST_OP', index => $index, value => \@expr, next => $pos };
}

sub parse_single_arg {
    my ( $tokens, $index ) = @_;
    return parse_fail() if $tokens->[$index][0] == COMMA();
    return parse_precedence_expression( $tokens, $index, $PRECEDENCE{','} + 1 );
}

sub parse_single_arg_array {
    my ( $tokens, $index ) = @_;
    my $ast = parse_single_arg( $tokens, $index );
    return $ast         if $ast->{FAIL};
    return parse_fail() if $ast->{type} eq 'PREFIX_OP' && $ast->{value}{op} ne '@';
    return $ast;
}

#----------------------

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

my %CORE_OP_GRAMMAR = (

    # define placeholder parsers for Sub-Languages that we don't have yet
    meta_parse_using(
        [qw{ qq qx qr <> }],
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
            ## return parse_arg_list( $tokens, $index, type => "${name}_OP", signature => '' );
            return meta_grammar(
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
        [qw{ goto }],
        sub {
            my ( $tokens, $index, $name ) = @_;
            return meta_grammar(
                $tokens, $index,
                meta_optional_parenthesis(
                    {
                        type => "${name}_OP",
                        opt  => [ \&parse_bareword, \&parse_single_arg, ]
                    },
                ),
            );
        }
    ),
    meta_parse_using(
        [qw{ next redo last }],
        sub {
            my ( $tokens, $index, $name ) = @_;
            return meta_grammar(
                $tokens, $index,
                meta_optional_parenthesis(
                    {
                        type => "${name}_OP",
                        opt  => [ \&parse_bareword, \&parse_single_arg, { seq => [] }, ]
                    },
                ),
            );
        }
    ),
    meta_parse_using(
        [qw{ pop shift }],
        sub {
            my ( $tokens, $index, $name ) = @_;
            return meta_grammar(
                $tokens, $index,
                meta_optional_parenthesis(
                    {
                        type => "${name}_OP",
                        opt  => [ \&parse_single_arg_array, { seq => [] }, ],
                    }
                ),
            );
        }
    ),
    meta_parse_using(
        [qw{ push unshift }],
        sub {
            my ( $tokens, $index, $name ) = @_;
            return meta_grammar(
                $tokens, $index,
                meta_optional_parenthesis(
                    {
                        type => "${name}_OP",
                        seq  => [ \&parse_single_arg_array, { opt => [ \&parse_arg_list, { seq => [] }, ] } ],
                    }
                ),
            );
        }
    ),
    meta_parse_using(
        [
            'abs',          'alarm',       'caller',         'chdir',            'chomp',     'chop',
            'chr',          'chroot',      'cos',            'defined',          'delete',    'eval',
            'exists',       'exit',        'exp',            'getgrgid',         'getgrnam',  'gethostbyname',
            'getnetbyname', 'getpgrp',     'getprotobyname', 'getprotobynumber', 'getpwnam',  'getpwuid',
            'gmtime',       'hex',         'int',            'lc',               'lcfirst',   'length',
            'localtime',    'log',         'oct',            'ord',              'quotemeta', 'rand',
            'readlink',     'reset',       'readpipe',       'ref',              'rmdir',     'sethostent',
            'setnetent',    'setprotoent', 'setservent',     'sin',              'sleep',     'sqrt',
            'srand',        'uc',          'ucfirst',        'umask',            'undef',
        ],
        sub {
            my ( $tokens, $index, $name ) = @_;
            return meta_grammar(
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
            return meta_grammar(
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
            return meta_grammar(
                $tokens, $index,
                {
                    type => "${name}_OP",
                    seq  => [
                        \&parse_single_arg,    # XXX my EXPR    my (LIST)
                    ],
                },
            );
        },
    ),
    meta_parse_using(
        [qw{ die open }],
        sub {
            my ( $tokens, $index, $name ) = @_;
            return meta_grammar(
                $tokens, $index,
                meta_optional_parenthesis(
                    {
                        type => "${name}_OP",
                        opt  => [
                            \&parse_arg_list,    # die LIST
                            { seq => [] },       # die
                        ],
                    }
                ),
            );
        },
    ),

    meta_parse_using(
        [qw{ return }],
        sub {
            # Unlike most named operators, this is also exempt from the
            # looks-like-a-function rule, so "return ("foo")."bar"" will cause
            # "bar" to be part of the argument to "return".

            my ( $tokens, $index, $name ) = @_;
            return meta_grammar(
                $tokens, $index,
                {
                    type => "${name}_OP",
                    opt  => [
                        \&parse_arg_list,    # return LIST
                        { seq => [] },       # return
                    ],
                }
            );
        },
    ),
    meta_parse_using(
        [qw{ print printf say }],
        sub {
            my ( $tokens, $index, $name ) = @_;
            return meta_grammar(
                $tokens, $index,
                meta_optional_parenthesis(
                    {
                        type => "${name}_OP",
                        opt  => [
                            {    # print BLOCK LIST
                                seq => [ $rule_block, \&parse_optional_whitespace, \&parse_arg_list, ]
                            },
                            {    # print FILE LIST
                                seq => [ \&parse_file_handle, \&parse_optional_whitespace, \&parse_arg_list ]
                            },
                            \&parse_arg_list,    # print LIST
                            { seq => [] },       # print
                        ],
                    }
                ),
            );
        },
    ),
    'do' => sub {
        my ( $tokens, $index, $name ) = @_;
        return meta_grammar(
            $tokens, $index,
            meta_optional_parenthesis(
                {
                    type => "${name}_OP",
                    opt  => [ $rule_block, \&parse_single_arg, ]
                }
            )
        );
    },
    'eval' => sub {
        my ( $tokens, $index, $name ) = @_;
        return meta_grammar(
            $tokens, $index,
            meta_optional_parenthesis(
                {
                    type => "${name}_OP",
                    opt  => [
                        $rule_block,    # eval BLOCK
                        \&parse_single_arg, { seq => [] },
                    ],
                },
            ),
        );
    },
    'sub' => sub {
        my ( $tokens, $index, $name ) = @_;
        return meta_grammar(
            $tokens, $index,
            {
                type => "${name}_OP",
                seq  => [ \&parse_statement_block, ],
            },
        );
    },
    meta_parse_using(
        [qw{ use no }],
        sub {
            my ( $tokens, $index, $name ) = @_;
            return meta_grammar(
                $tokens, $index,
                {
                    type => "${name}_OP",
                    seq  => [ \&parse_single_arg, \&parse_optional_whitespace, \&parse_arg_list, ]    # NAME LIST
                }
            );
        }
    ),
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
    return $message . ' at ' . env_get_current_filename($tokens) . ' line ' . $line . ', near ' . error_message_quote( join( '', @near ) ) . "\n";
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
    my $pos      = $index;
    my $op_value = $tokens->[$pos][1];
    my $type     = $tokens->[$pos][0];
    my $left_expr;

    if ( $LIST{$op_value} ) {
        $left_expr = parse_arg_list( $tokens, $pos );
        return parse_fail() if $left_expr->{FAIL};
    }
    elsif ( $PREFIX{$op_value} ) {
        $pos++;
        $pos = parse_optional_whitespace( $tokens, $pos )
          if $START_WHITESPACE{ $tokens->[$pos][0] };
        if ( $type == SIGIL() && ( $tokens->[$pos][0] == IDENTIFIER() || $tokens->[$pos][0] == DOUBLE_COLON() ) ) {    # $name
            my $ast = parse_colon_bareword( $tokens, $pos );
            $left_expr = { type => 'PREFIX_OP', value => { op => $op_value, arg => $ast }, next => $ast->{next} };
        }
        else {
            my $expr = parse_precedence_expression( $tokens, $pos, $PRECEDENCE{$op_value} );
            return parse_fail() if $expr->{FAIL};
            if (   $NON_ASSOC_AUTO{$op_value}
                && ( $expr->{type} eq 'POSTFIX_OP' || $expr->{type} eq 'PREFIX_OP' )
                && $NON_ASSOC_AUTO{ $expr->{value}{op} } )    # check for nonassoc syntax error
            {
                error( $tokens, $index );
            }
            $left_expr = { type => 'PREFIX_OP', value => { op => $op_value, arg => $expr }, next => $expr->{next} };
        }
    }
    else {
        $left_expr = parse_term( $tokens, $index );
        return parse_fail() if $left_expr->{FAIL};
    }

  EXPR:
    while (1) {
        $pos = $left_expr->{next};
        $pos++ if $tokens->[$pos][0] == WHITESPACE();
        $pos = parse_optional_whitespace( $tokens, $pos )
          if $START_WHITESPACE{ $tokens->[$pos][0] };

        return $left_expr if $tokens->[$pos][0] == END_TOKEN();
        $op_value = $tokens->[$pos][1];
        $type     = $tokens->[$pos][0];
        my $op_pos = $pos;

        return $left_expr unless exists $INFIX{$op_value} || exists $POSTFIX{$op_value};

        # TODO .. and ... are "nonassoc"

        my $precedence = $PRECEDENCE{$op_value};
        return $left_expr if $precedence < $min_precedence;

        $pos++;
        $pos++ if $tokens->[$pos][0] == WHITESPACE();
        $pos = parse_optional_whitespace( $tokens, $pos )
          if $START_WHITESPACE{ $tokens->[$pos][0] };

        if ( $LIST{$op_value} ) {
            $left_expr = parse_arg_list( $tokens, $pos, first => $left_expr );
        }
        elsif ( $type == PAREN_OPEN() || $type == CURLY_OPEN() || $type == SQUARE_OPEN() ) {    # Handle postfix () [] {}
            my $right_expr = parse_term( $tokens, $op_pos );
            return parse_fail() if $right_expr->{FAIL};
            return parse_fail()
              if $type == PAREN_OPEN()
              && $left_expr->{type} eq 'PREFIX_OP'
              && $FORBIDDEN_CALL{ $left_expr->{value}{op} };                                    # $a()  is forbidden
            return parse_fail()
              if $type == PAREN_OPEN()
              && $left_expr->{type} eq 'APPLY_OR_DEREF'
              && $left_expr->{value}{op} eq '(';                                                # expr()()  is forbidden
            $left_expr = { type => 'APPLY_OR_DEREF', value => { op => $op_value, arg => [ $left_expr, $right_expr ] }, next => $right_expr->{next} };
        }
        elsif ( $type == ARROW() ) {                                                            # Handle ->method  ->method()  ->() ->[] ->{}
            $type = $tokens->[$pos][0];
            my $right_expr = parse_precedence_expression( $tokens, $pos, $PRECEDENCE{'->'} + 1 );
            return parse_fail() if $right_expr->{FAIL};
            if ( $type == PAREN_OPEN() || $type == CURLY_OPEN() || $type == SQUARE_OPEN() ) {    # Handle ->() ->[] ->{}
                $left_expr =
                  { type => 'APPLY_OR_DEREF', value => { op => $op_value, arg => [ $left_expr, $right_expr ] }, next => $right_expr->{next} };
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
        }    # /arrow
        elsif ( $type == QUESTION() ) {    # Handle ternary operator
            my $true_expr = parse_precedence_expression( $tokens, $pos, 0 );                    # Parse the true branch
            return parse_fail() if $true_expr->{FAIL};
            $pos = parse_optional_whitespace( $tokens, $true_expr->{next} );
            return parse_fail() if $tokens->[$pos][0] != COLON();
            $pos = parse_optional_whitespace( $tokens, $pos + 1 );
            my $false_expr = parse_precedence_expression( $tokens, $pos, $PRECEDENCE{'?'} );    # Parse the false branch
            $left_expr = { type => 'TERNARY_OP', value => [ '?', $left_expr, $true_expr, $false_expr ], next => $false_expr->{next} };
        }
        elsif ( $POSTFIX{$op_value} ) {                                                         # Handle postfix operators
            if (   $NON_ASSOC_AUTO{$op_value}
                && ( $left_expr->{type} eq 'POSTFIX_OP' || $left_expr->{type} eq 'PREFIX_OP' )
                && $NON_ASSOC_AUTO{ $left_expr->{value}{op} } )                                 # check for nonassoc syntax error
            {
                error( $tokens, $index );
            }
            $left_expr = { type => 'POSTFIX_OP', value => { op => $op_value, arg => $left_expr }, next => $pos };
        }
        else {
            my $next_min_precedence = $ASSOC_RIGHT{$op_value} ? $precedence : $precedence + 1;
            my $right_expr          = parse_precedence_expression( $tokens, $pos, $next_min_precedence );
            return parse_fail() if $right_expr->{FAIL};
            $left_expr = { type => 'BINARY_OP', value => [ $op_value, $left_expr, $right_expr ], next => $right_expr->{next} };
        }
    }    # /EXPR
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
                    $processed_ast = $CORE_OP_GRAMMAR{q}->( $tokens, $index, 'q', $here_doc );
                }
                elsif ( $quote eq '"' ) {
                    $processed_ast = $CORE_OP_GRAMMAR{qq}->( $tokens, $index, 'qq', $here_doc );
                }
                elsif ( $quote eq '`' ) {
                    $processed_ast = $CORE_OP_GRAMMAR{qx}->( $tokens, $index, 'qx', $here_doc );
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

sub parse_bareword {    # next LABEL
    my ( $tokens, $index ) = @_;
    return parse_fail() if $RESERVED_WORDS{ $tokens->[$index][1] };
    return { type => 'BAREWORD', index => $index, value => $tokens->[$index][1], next => $index + 1 }
      if $tokens->[$index][0] == IDENTIFIER();
    return parse_fail();
}

sub fully_qualified_name {
    my ( $namespace, $name ) = @_;

    ## if ($namespace =~ /^::/) {
    ##     $namespace = "main" . $namespace;
    ## }

    # Check if the name name is fully qualified
    if ( $name =~ /^::/ ) {

        # Normalize ::sub to main::sub
        return "main" . $name;
    }
    elsif ( $name =~ /::/ ) {

        # If the name contains '::', it's already fully qualified
        return $name;
    }
    else {
        # Otherwise, combine the namespace and the name
        return $namespace . '::' . $name;
    }
}

sub parse_colon_bareword {
    my ( $tokens, $index ) = @_;
    my $pos = $index;
    my @tok;
    ## push @tok, 'main' if $tokens->[$pos][0] == DOUBLE_COLON();  # ::  is  main::
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
    $pos++ if $tokens->[$pos][0] == WHITESPACE();
    $pos = parse_optional_whitespace( $tokens, $pos )
      if $START_WHITESPACE{ $tokens->[$pos][0] };
    if ( $tokens->[$pos][1] ne $end_delim ) {
        if ( $start_delim eq '{' && $tokens->[$pos][0] == IDENTIFIER() ) {    # { bareword }
            my $pos1 = parse_optional_whitespace( $tokens, $pos + 1 );
            if ( $tokens->[$pos1][1] eq $end_delim ) {
                return {
                    type  => 'PAREN',
                    value => { delimiter => $start_delim, args => { type => 'BAREWORD', value => $tokens->[$pos][1] } },
                    next  => $pos1 + 1
                };
            }
        }
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
        $pos++                   if $tokens->[$pos][0] == WHITESPACE();
        $pos = parse_optional_whitespace( $tokens, $pos )
          if $START_WHITESPACE{ $tokens->[$pos][0] };
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
    #   print $f LIST
    #   print STDOUT LIST
    #   print STDOUT;
    #   print ::STDOUT
    #   print $f +3;        # plus is close to the second argument
    #   print $f or die;    # `or` is not an argument to print
    #   print STDOUT (10);  # not a function call
    #   print $fh(10);      # not a function call
    #
    #   print {*STDERR} 123 # block syntax - see the 'print/printf/say' grammar
    #   print {*STDERR;} 123
    #   print {;*STDERR;} 123
    #   print {$f} LIST
    #   print {STDOUT} LIST
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
    my $value = $tokens->[$pos][1];
    if ( $tokens->[$pos][0] == IDENTIFIER() ) {
        return parse_fail() if exists $INFIX{$value} || exists $PREFIX{$value};    # check that the bareword is not a builtin like `chr`
        $ast = { type => 'BAREWORD', value => $tokens->[$pos][1], next => $pos + 1 };
    }
    elsif ( $tokens->[$pos][0] == SIGIL() && $value eq '$' ) {
        $ast = parse_precedence_expression( $tokens, $pos, $PRECEDENCE{'$'} );
    }
    return parse_fail() if !$ast || $ast->{FAIL};
    $pos = $ast->{next};
    $ast = { type => 'FILE_HANDLE', value => $ast, next => $pos };

    # we have a symbol; now check the syntax that follows the filehandle

    # TODO check for terminators: END_TOKEN, SEMICOLON, PAREN_CLOSE, CURLY_CLOSE, SQUARE_CLOSE

    return parse_fail() if !$START_WHITESPACE{ $tokens->[$pos][0] };                 # must be followed by space or a comment
    $pos = parse_optional_whitespace( $tokens, $pos );
    my $tok = $tokens->[$pos][1];
    return parse_fail() if $INFIX{$tok} && $PRECEDENCE{$tok} >= $PRECEDENCE{','};    # must not be followed by a higher precedence infix operator
    return $ast;
}

sub parse_term {
    my ( $tokens, $index ) = @_;
    my $pos  = $index;
    my $type = $tokens->[$pos][0];
    my $ast;
    if ( $type == IDENTIFIER() ) {

        if ( $tokens->[ $pos + 1 ][0] == DOUBLE_COLON() ) {
            return parse_colon_bareword( $tokens, $index );    # TODO parse special cases like CORE::print
        }

        my $stmt = $tokens->[$pos][1];
        $pos++;
        $pos++ if $tokens->[$pos][0] == WHITESPACE();
        $pos = parse_optional_whitespace( $tokens, $pos )
          if $START_WHITESPACE{ $tokens->[$pos][0] };
        if ( $tokens->[$pos][0] == FAT_ARROW() ) {    # bareword
            return { type => 'STRING', value => $tokens->[$index][1], next => $index + 1 };
        }

        return parse_fail() if $RESERVED_WORDS{$stmt};

        if ( exists $CORE_OP_GRAMMAR{$stmt} ) {       # apply a built-in function
            return $CORE_OP_GRAMMAR{$stmt}->( $tokens, $pos, $stmt );
        }
        if ( my $subr = env_get_subroutine( $tokens, $stmt ) ) {    # name ARGS     apply a predeclared sub
            my $signature = $subr->{signature}{value};
            my $args      = parse_arg_list( $tokens, $pos, signature => $signature, sub_name => $stmt );    # name(ARGS)  name ARGS
            return parse_fail() if $args->{FAIL};
            return { type => 'APPLY_DECLARED_SUB', value => { name => $stmt, args => $args }, next => $args->{next} };
        }
        if ( $tokens->[$pos][0] == PAREN_OPEN() ) {                                                         # name(ARGS)     apply an unknown sub
            my $args = parse_delimited_expression( $tokens, $pos + 1, '(', ')' );
            return parse_fail() if $args->{FAIL};
            return { type => 'APPLY_UNKNOWN_SUB', value => { name => $stmt, args => $args }, next => $args->{next} };
        }
        return { type => 'BAREWORD', value => $tokens->[$index][1], next => $index + 1 };
    }
    elsif ( $type == NUMBER() || $type == DOT() ) {
        return parse_number( $tokens, $index );
    }
    elsif ( $type == STRING_DELIM() ) {
        my $quote = $tokens->[$index][1];
        if ( $quote eq "'" ) {
            return $CORE_OP_GRAMMAR{q}->( $tokens, $index, 'q' );
        }
        elsif ( $quote eq '"' ) {
            return $CORE_OP_GRAMMAR{qq}->( $tokens, $index, 'qq' );
        }
        elsif ( $quote eq '`' ) {
            return $CORE_OP_GRAMMAR{qx}->( $tokens, $index, 'qx' );
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
        return $CORE_OP_GRAMMAR{'<>'}->( $tokens, $index, '<>' );    # <...>
    }
    elsif ( $type == SLASH() ) {                                     # /.../
        return $CORE_OP_GRAMMAR{m}->( $tokens, $index, 'm' );
    }
    elsif ( $type == LESS_LESS() ) {                                 # here doc:  <<   <<~
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
    $pos++ if $tokens->[$pos][0] == WHITESPACE();
    $pos = parse_optional_whitespace( $tokens, $pos )
      if $START_WHITESPACE{ $tokens->[$pos][0] };
    return parse_fail() if $tokens->[$pos][0] == END_TOKEN();

    my $pos0 = $pos;
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
            $pos = $pos1;
            $ast = { type => 'STATEMENT', value => [] };
            my $var;
            if ( $tokens->[$pos][1] eq 'my' ) {
                error( $tokens, $index ) if $stmt ne 'for' && $stmt ne 'foreach';
                $pos = parse_optional_whitespace( $tokens, $pos + 1 );
                $var = parse_precedence_expression( $tokens, $pos, $PRECEDENCE{'$'} );
                $pos = parse_optional_whitespace( $tokens, $var->{next} + 1 );
            }
            my $expr = parse_for_expression( $tokens, $pos );
            if ( $expr->{value}{delimiter} eq ';' ) {
                error( $tokens, $index ) if $var;                                   # don't mix:  for my ...   and   for ( ; ; ) ...
                error( $tokens, $index ) if $stmt ne 'for' && $stmt ne 'foreach';
                my @expr = ($expr);
                $expr = parse_for_expression( $tokens, $expr->{next} - 1 );
                push @expr, $expr;
                $expr = parse_for_expression( $tokens, $expr->{next} - 1 );
                error( $tokens, $index ) if ( $expr->{value}{delimiter} ne ')' );
                push @expr, $expr;
                $expr = { type => 'THREE_ARG_FOR', value => \@expr, next => $expr->{next} };
            }
            error( $tokens, $index ) if $expr->{FAIL};
            $pos = parse_optional_whitespace( $tokens, $expr->{next} );
            my $block = parse_statement_block( $tokens, $pos );
            $pos = $block->{next};
            push @{ $ast->{value} }, { stmt => $stmt, var => $var, expr => $expr, block => $block };

            $pos = parse_optional_whitespace( $tokens, $pos );

            # Handle 'elsif', 'else', and 'continue' based on the value of $stmt
            if ( $stmt eq 'if' || $stmt eq 'unless' ) {
              ELSIF:
                while (1) {
                    if ( $tokens->[$pos][1] eq 'elsif' ) {
                        $pos = parse_optional_whitespace( $tokens, $pos + 1 );
                        my $expr = parse_for_expression( $tokens, $pos );
                        $pos = parse_optional_whitespace( $tokens, $expr->{next} );
                        my $block = parse_statement_block( $tokens, $pos );
                        $pos = $block->{next};
                        push @{ $ast->{value} }, { stmt => 'elsif', expr => $expr, block => $block };
                        $pos = parse_optional_whitespace( $tokens, $pos );
                        next ELSIF;
                    }
                    if ( $tokens->[$pos][1] eq 'else' ) {
                        $pos = parse_optional_whitespace( $tokens, $pos + 1 );
                        my $block = parse_statement_block( $tokens, $pos );
                        $pos = $block->{next};
                        push @{ $ast->{value} }, { stmt => 'else', block => $block };
                        $pos = parse_optional_whitespace( $tokens, $pos );
                    }
                    last ELSIF;
                }
            }
            elsif ( $tokens->[$pos][1] eq 'continue' ) {
                $pos = parse_optional_whitespace( $tokens, $pos + 1 );
                my $block = parse_statement_block( $tokens, $pos );
                $pos = $block->{next};
                push @{ $ast->{value} }, { stmt => 'continue', block => $block };
            }
            $ast->{next} = $pos;
        }
        elsif ( $stmt eq 'sub' ) {
            $pos = $pos1;
            if ( $tokens->[$pos][0] == IDENTIFIER() ) {    # sub NAME
                my $name = $tokens->[$pos][1];
                $pos = parse_optional_whitespace( $tokens, $pos + 1 );
                my $signature;
                if ( $tokens->[$pos][0] == PAREN_OPEN() ) {    # sub NAME ()   signature
                    $signature = $CORE_OP_GRAMMAR{q}->( $tokens, $pos, 'q' );                # get the raw text
                    $pos       = parse_optional_whitespace( $tokens, $signature->{next} );
                }

                # TODO use: fully_qualified_name($namespace, $name)
                env_set_subroutine( $tokens, $name, { signature => $signature } );           # store the declaration

                my $block = parse_statement_block( $tokens, $pos );
                $ast = {
                    type  => 'NAMED_SUB',
                    value => { stmt => $stmt, name => $name, block => $block, signature => $signature },
                    next  => $block->{next}
                };
                $pos = $ast->{next};
            }
        }
        elsif ( $stmt eq 'package' ) {
            $pos = $pos1;
            $ast = parse_colon_bareword( $tokens, $pos );
            error( $tokens, $index ) if $ast->{FAIL};
            $pos = $ast->{next};
            $pos = parse_optional_whitespace( $tokens, $pos );

            # TODO - package version
            my $version;
            my $old_package = env_get_package($tokens);
            env_set_package( $tokens, { name => $ast, version => $version } );
            my $block;
            if ( $tokens->[$pos][0] == CURLY_OPEN() ) {
                $block = parse_statement_block( $tokens, $pos );
                env_set_package( $tokens, $old_package );
                $pos = $block->{next};
            }
            $ast = {
                type  => 'PACKAGE',
                value => { name => $ast, block => $block, version => $version },
                next  => $pos,
            };
        }
        elsif ( $stmt eq 'else' || $stmt eq 'elsif' || $stmt eq 'continue' ) {
            error( $tokens, $index );
        }
    }    # /IDENTIFIER
    elsif ( $tokens->[$pos][0] == CURLY_OPEN() ) {

        # TODO - BEGIN, END, INIT, CHECK, UNITCHECK

        $ast = parse_statement_block( $tokens, $pos );

        # TODO continue

        $pos = $ast->{next};
    }
    if ( !$ast ) {
        $pos = $pos0;
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
    my $filename  = "<DATA>";
    my $perl_code = join( '', <DATA> );

    my $args = shift @ARGV // '';
    if ( $args eq '-e' ) {
        $filename  = "-e";
        $perl_code = shift(@ARGV) . "\n";
    }
    elsif ($args) {
        $filename = $args;
        open my $f, "<", $args or die "Can't open file '$args'";
        $perl_code = join( "", <$f> );
    }

    my $tokens = tokenize($perl_code);
    env_set_current_filename( $tokens, $filename );

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

say $x# comment
[123];

$::x = 13;

__END__
123

