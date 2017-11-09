use v5;

package Perlito5::Grammar::String;

use Perlito5::Grammar::Precedence;

token term_q_quote {
    [ 'q' [ '#' | <.Perlito5::Grammar::Space::opt_ws> <!before '=>' > . ]
    | \'
    ]
    <q_quote_parse>
        { $MATCH->{capture} = [ 'term', Perlito5::Match::flat($MATCH->{q_quote_parse}) ]  }
};
token term_qq_quote {
    [ 'qq' [ '#' | <.Perlito5::Grammar::Space::opt_ws> <!before '=>' > . ]
    | '"'
    ]
    <qq_quote_parse>
        { $MATCH->{capture} = [ 'term', Perlito5::Match::flat($MATCH->{qq_quote_parse}) ]  }
};
token term_qw_quote {
    'qw' [ '#' | <.Perlito5::Grammar::Space::opt_ws> <!before '=>' > . ]
    <qw_quote_parse>
        { $MATCH->{capture} = [ 'term', Perlito5::Match::flat($MATCH->{qw_quote_parse}) ]  }
};
token term_m_quote {
    [ 'm' [ '#' | <.Perlito5::Grammar::Space::opt_ws> <!before '=>' > . ]
    | '/' 
    ]
    <m_quote_parse>
        {
            $MATCH->{capture} = [ 'term', Perlito5::Match::flat($MATCH->{m_quote_parse}) ]  
        }
};
token term_s_quote {
    's' [ '#' | <.Perlito5::Grammar::Space::opt_ws> <!before '=>' > . ]
    <s_quote_parse>
        { 
            $MATCH->{capture} = [ 'term', Perlito5::Match::flat($MATCH->{s_quote_parse}) ]  
        }
};
token term_qx {
    [ 'qx' [ '#' | <.Perlito5::Grammar::Space::opt_ws> <!before '=>' > . ] 
    | '`'
    ]
    <qx_quote_parse>
        { $MATCH->{capture} = [ 'term', Perlito5::Match::flat($MATCH->{qx_quote_parse}) ]  }
};
token term_glob {
    '<' <glob_quote_parse>
        { $MATCH->{capture} = [ 'term', Perlito5::Match::flat($MATCH->{glob_quote_parse}) ]  }
};
token term_tr_quote {
    [ 'tr' | 'y' ] [ '#' | <.Perlito5::Grammar::Space::opt_ws> <!before '=>' > . ]
    <tr_quote_parse>
        { 
            $MATCH->{capture} = [ 'term', Perlito5::Match::flat($MATCH->{tr_quote_parse}) ]  
        }
};
token term_qr_quote {
    'qr' [ '#' | <.Perlito5::Grammar::Space::opt_ws> <!before '=>' > . ]
    <qr_quote_parse>
        { 
            $MATCH->{capture} = [ 'term', Perlito5::Match::flat($MATCH->{qr_quote_parse}) ]  
        }
};

our $unicode_table;     # unicore/UnicodeData.txt
sub get_unicode_table {
    $unicode_table = Perlito5::Grammar::Use::slurp_file("unicore/UnicodeData.txt")
        if !$unicode_table;
    return $unicode_table;
}

my %pair = (
    '{' => '}',
    '(' => ')',
    '[' => ']',
    '<' => '>',
);

my %escape_sequence = qw/ a 7 b 8 e 27 f 12 n 10 r 13 t 9 /;

my %hex   = map +($_ => 1), qw/ 0 1 2 3 4 5 6 7 8 9 A B C D E F /;
my %octal = map +($_ => 1), qw/ 0 1 2 3 4 5 6 7 /;

sub q_quote_parse {
    my $str = $_[0];
    my $pos = $_[1];
    my $delimiter = $str->[$pos-1];
    my $open_delimiter = $delimiter;
    $delimiter = $pair{$delimiter} if exists $pair{$delimiter};
    return string_interpolation_parse($str, $pos, $open_delimiter, $delimiter, 0);
}
sub qq_quote_parse {
    my $str = $_[0];
    my $pos = $_[1];
    my $delimiter = $str->[$pos-1];
    my $open_delimiter = $delimiter;
    $delimiter = $pair{$delimiter} if exists $pair{$delimiter};
    return string_interpolation_parse($str, $pos, $open_delimiter, $delimiter, 1);
}
sub qw_quote_parse {
    my $str = $_[0];
    my $pos = $_[1];
    my $delimiter = $str->[$pos-1];
    my $open_delimiter = $delimiter;
    $delimiter = $pair{$delimiter} if exists $pair{$delimiter};

    my $m = string_interpolation_parse($str, $pos, $open_delimiter, $delimiter, 0);
    if ( $m ) {
        $m->{capture} = Perlito5::AST::Apply->new(
                code      => 'list:<,>',
                arguments => [ map( Perlito5::AST::Buf->new( buf => $_ ), split(' ', Perlito5::Match::flat($m)->{buf})) ],
                namespace => '',
            );
    }
    return $m;
}
sub m_quote_parse {
    my $str = $_[0];
    my $pos = $_[1];
    my $delimiter = $str->[$pos-1];
    my $open_delimiter = $delimiter;
    my $closing_delimiter = $delimiter;
    $closing_delimiter = $pair{$delimiter} if exists $pair{$delimiter};
    my $interpolate = 2;
    $interpolate = 3 if $delimiter eq "'";

    # TODO - call the regex compiler
    my $part1 = string_interpolation_parse($str, $pos, $open_delimiter, $closing_delimiter, $interpolate);
    return $part1 unless $part1;
    my $str_regex = $part1->{capture};

    my $p = $part1->{to};
    my $modifiers = '';
    my $m = Perlito5::Grammar::ident($str, $p);
    if ( $m ) {
        $modifiers = Perlito5::Match::flat($m);
        $part1->{to} = $m->{to};
    }

    $part1->{capture} = Perlito5::AST::Apply->new(
        code      => 'p5:m',
        arguments => [
            $str_regex,
            Perlito5::AST::Buf->new( buf => $modifiers ),
            Perlito5::AST::Var::SCALAR_ARG(),
        ],
        namespace => ''
    );
    return $part1;
}
sub s_quote_parse {
    my $str = $_[0];
    my $pos = $_[1];
    my $delimiter = $str->[$pos-1];
    my $open_delimiter = $delimiter;
    my $closing_delimiter = $delimiter;
    $closing_delimiter = $pair{$delimiter} if exists $pair{$delimiter};

    my $interpolate = 2;
    $interpolate = 3 if $delimiter eq "'";
    my $part1 = string_interpolation_parse($str, $pos, $open_delimiter, $closing_delimiter, $interpolate);
    return $part1 unless $part1;

    # TODO - call the regex compiler
    my $str_regex = Perlito5::Match::flat($part1);

    my $part2;
    my $m;
    my $p = $part1->{to};
    if ( exists $pair{$delimiter} ) {
        # warn "pair delimiter $delimiter at $p";
        $m = Perlito5::Grammar::Space::opt_ws($str, $p);
        $p = $m->{to};
        $delimiter = $str->[$p];
        $open_delimiter = $delimiter;
        $p++;
        # warn "second delimiter $delimiter";
        $closing_delimiter = $delimiter;
        $closing_delimiter = $pair{$delimiter} if exists $pair{$delimiter};
    }
    # scan for closing delimiter
    $part2 = string_interpolation_parse($str, $p, $open_delimiter, $closing_delimiter, 0);
    $part2 || return $part2;
    my @replace = @{$str}[ $p .. $part2->{to} - 2 ];
    my $replace;

    # read the modifiers
    $p = $part2->{'to'};
    my $modifiers = '';
    $m = Perlito5::Grammar::ident($str, $p);
    if ($m) {
        $modifiers = Perlito5::Match::flat($m);
    }

    if ($modifiers =~ /e/) {
        # $part2 is code
        my $m = Perlito5::Grammar::block( [ '{', @replace, '}' ], 0);  # parse the block
        if (!$m) {
            Perlito5::Compiler::error "syntax error";
        }
        $replace = Perlito5::Match::flat($m);
        if ($modifiers =~ /ee/) {
            # { eval do { ... } }
            $replace = Perlito5::AST::Block->new(
                            'sig' => undef,
                            'stmts' => [
                                Perlito5::AST::Apply->new(
                                    'code' => 'eval',
                                    'arguments' => [
                                        Perlito5::AST::Apply->new(
                                            'code' => 'do',
                                            'arguments' => [$replace],
                                        ),
                                    ],
                                    'bareword' => '',
                                    'namespace' => '',
                                ),
                            ]
                        );
        }
    }
    else {
        # $part2 is string
        $interpolate = 1;
        $delimiter eq chr(39) && ($interpolate = 3);
        my $m = string_interpolation_parse( [ $open_delimiter, @replace, $closing_delimiter ], 1, $open_delimiter, $closing_delimiter, $interpolate);
        if (!$m) {;
            Perlito5::Compiler::error('syntax error')
        }
        $replace = Perlito5::Match::flat($m);
    }

    if ($m) {
        $part2->{'to'} = $m->{'to'}
    }
    $part2->{capture} = Perlito5::AST::Apply->new(
        code      => 'p5:s',
        arguments => [
            $str_regex,
            $replace,
            Perlito5::AST::Buf->new( buf => $modifiers ),
            Perlito5::AST::Var::SCALAR_ARG(),
        ],
        namespace => ''
    );
    return $part2;
}
sub qr_quote_parse {
    my $str = $_[0];
    my $pos = $_[1];
    my $delimiter = $str->[ $pos-1 ];
    my $open_delimiter = $delimiter;
    my $closing_delimiter = $delimiter;
    $closing_delimiter = $pair{$delimiter} if exists $pair{$delimiter};
    my $interpolate = 2;
    $interpolate = 3 if $delimiter eq "'";

    # TODO - call the regex compiler
    my $part1 = string_interpolation_parse($str, $pos, $open_delimiter, $closing_delimiter, $interpolate);
    return $part1 unless $part1;
    my $str_regex = $part1->{capture};

    my $p = $part1->{to};
    my $modifiers = '';
    my $m = Perlito5::Grammar::ident($str, $p);
    if ( $m ) {
        $modifiers = Perlito5::Match::flat($m);
        $part1->{to} = $m->{to};
    }

    $part1->{capture} = Perlito5::AST::Apply->new(
        code      => 'p5:qr',
        arguments => [
            $str_regex,
            Perlito5::AST::Buf->new( buf => $modifiers ),
        ],
        namespace => ''
    );
    return $part1;
}
sub qx_quote_parse {
    my $str = $_[0];
    my $pos = $_[1];
    my $delimiter = $str->[$pos-1];
    my $open_delimiter = $delimiter;
    $delimiter = $pair{$delimiter} if exists $pair{$delimiter};

    my $m = string_interpolation_parse($str, $pos, $open_delimiter, $delimiter, 0);
    if ( $m ) {
        $m->{capture} = Perlito5::AST::Apply->new(
                code      => 'qx',
                arguments => [ Perlito5::Match::flat($m) ],
                namespace => '',
            );
    }
    return $m;
}
sub glob_quote_parse {
    my $str = $_[0];
    my $pos = $_[1];
    my $delimiter = $str->[$pos-1];
    my $open_delimiter = $delimiter;
    $delimiter = $pair{$delimiter} if exists $pair{$delimiter};
    # Special cases:
    # <>                 - no arguments, read from @ARGV
    # <<>>               - no arguments, read from @ARGV using 3-args open
    # <FILE>             - file
    # <$var>             - file
    # < anything else >  - is a glob() call

    if ( $str->[$pos] eq '<' && $str->[$pos+1] eq '>' && $str->[$pos+2] eq '>' ) {
        # <<>>
        return {
            str  => $str, 
            from => $pos, 
            to   => $pos + 3, 
            capture => Perlito5::AST::Apply->new(
                code      => 'readline',
                arguments => [
                    Perlito5::AST::Apply->new(
                        code      => '<>',
                        arguments => [],
                        namespace => '',
                        bareword  => 1,
                    )
                ],
                namespace => '',
            ),
        };
    }

    if ( $str->[$pos] eq '>' ) {
        return {
            str  => $str, 
            from => $pos, 
            to   => $pos + 1, 
            capture => Perlito5::AST::Apply->new(
                code      => 'readline',
                arguments => [],
                namespace => '',
            ),
        };
    }


    my $p = $pos;
    my $sigil = '::';
    if ( $str->[$p] eq '$' ) {
        $sigil = '$';
        $p++;
    }
    my $m_namespace = Perlito5::Grammar::optional_namespace_before_ident( $str, $p );
    my $namespace = Perlito5::Match::flat($m_namespace);
    $p = $m_namespace->{to};
    my $m_name      = Perlito5::Grammar::ident( $str, $p );
    if ($m_name && $str->[$m_name->{to}] eq '>' ) {
        if ($sigil eq '::') {
            return {
                str  => $str, 
                from => $pos, 
                to   => $m_name->{to} + 1, 
                capture => Perlito5::AST::Apply->new(
                    code      => 'readline',
                    arguments => [
                        Perlito5::AST::Apply->new(
                            code      => Perlito5::Match::flat($m_name),
                            arguments => [],
                            namespace => $namespace,
                            bareword  => 1,
                        )
                    ],
                    namespace => '',
                ),
            };
        }
        return {
            str  => $str, 
            from => $pos, 
            to   => $m_name->{to} + 1, 
            capture => Perlito5::AST::Apply->new(
                code      => 'readline',
                arguments => [
                    Perlito5::AST::Var->new(
                        sigil     => $sigil,
                        name      => Perlito5::Match::flat($m_name),
                        namespace => $namespace,
                    )
                ],
                namespace => '',
            ),
        };
    }

    my $m = string_interpolation_parse($str, $pos, $open_delimiter, $delimiter, 1);
    if ( $m ) {
        $m->{capture} = Perlito5::AST::Apply->new(
                code      => 'glob',
                arguments => [ Perlito5::Match::flat($m) ],
                namespace => '',
            );
    }
    return $m;
}
sub tr_quote_parse {
    my $str = $_[0];
    my $pos = $_[1];
    my $delimiter = $str->[$pos-1];
    my $open_delimiter = $delimiter;
    my $closing_delimiter = $delimiter;
    $closing_delimiter = $pair{$delimiter} if exists $pair{$delimiter};

    my $interpolate = 2;
    $interpolate = 3 if $delimiter eq "'";
    my $part1 = string_interpolation_parse($str, $pos, $open_delimiter, $closing_delimiter, $interpolate);
    return $part1 unless $part1;

    # TODO - call the regex compiler
    my $str_regex = Perlito5::Match::flat($part1);

    my $part2;
    my $m;
    my $p = $part1->{to};
    if ( exists $pair{$delimiter} ) {
        # warn "pair delimiter $delimiter at $p";
        $m = Perlito5::Grammar::Space::opt_ws($str, $p);
        $p = $m->{to};
        $delimiter = $str->[$p];
        my $open_delimiter = $delimiter;
        $p++;
        # warn "second delimiter $delimiter";
        $closing_delimiter = $delimiter;
        $closing_delimiter = $pair{$delimiter} if exists $pair{$delimiter};

        $interpolate = 2;
        $interpolate = 3 if $delimiter eq "'";
        $part2 = string_interpolation_parse($str, $p, $open_delimiter, $closing_delimiter, $interpolate);
        return $part2 unless $part2;
    }
    else {
        $part2 = string_interpolation_parse($str, $p, $open_delimiter, $closing_delimiter, $interpolate);
        return $part2 unless $part2;
    }

    $p = $part2->{to};
    my $modifiers = '';
    $m = Perlito5::Grammar::ident($str, $p);
    if ( $m ) {
        $modifiers = Perlito5::Match::flat($m);
        $part2->{to} = $m->{to};
    }

    $part2->{capture} = Perlito5::AST::Apply->new(
        code      => 'p5:tr',
        arguments => [
            $str_regex,
            Perlito5::Match::flat($part2),
            Perlito5::AST::Buf->new( buf => $modifiers ),
            Perlito5::AST::Var::SCALAR_ARG(),
        ],
        namespace => ''
    );
    return $part2;
}

sub apply_quote_flags {
    my ($c, $quote_flags) = @_;
    return $c unless length($c);
    if ($quote_flags->{l}) {
        $c = lcfirst($c);
        delete $quote_flags->{l};
    }
    if ($quote_flags->{u}) {
        $c = ucfirst($c);
        delete $quote_flags->{u};
    }
    $c = lc($c) if $quote_flags->{L};
    $c = uc($c) if $quote_flags->{U};
    $c = quotemeta($c) if $quote_flags->{Q};
    return $c;
}

sub string_interpolation_parse {
    my $str            = $_[0];
    my $pos            = $_[1];
    my $open_delimiter = $_[2];
    my $delimiter      = $_[3];
    my $interpolate    = $_[4];  # 0 - single-quote; 1 - double-quote; 2 - regex; 3 - single-quote regex
    my $quote_flags    = $_[5] || {};  # lowercase/uppercase/quotemeta until /E or end-of-string

    my $interpolate_vars = ($interpolate == 1 || $interpolate == 2);

    my $p = $pos;

    my $balanced = $open_delimiter && exists $pair{$open_delimiter};

    my @args;
    my $buf = '';
    my $d1 = substr($delimiter, 0, 1);

    while (  $p < @$str
          && ! (  $str->[$p] eq $d1
               && join('', @{$str}[ $p .. $p + length($delimiter) - 1]) eq $delimiter
               )
          )
    {
        my $c = $str->[$p];
        my $c2 = $str->[$p+1];
        my $m;
        my $more = '';
        if ($balanced && $c eq '\\' && ($c2 eq $open_delimiter || $c2 eq $delimiter)) {
            $p++;
            $c = $c2;
        }
        elsif ($balanced && $c eq $open_delimiter) {
            $buf .= $c;
            $p++;
            $m = string_interpolation_parse($str, $p, $open_delimiter, $delimiter, $interpolate, $quote_flags);
            $more = $delimiter;
        }
        elsif ($interpolate_vars && ($c eq '$' || $c eq '@')) {
            my $match = Perlito5::Grammar::String::double_quoted_var( $str, $p, $delimiter, $interpolate );
            if ($match) {
                my $ast = $match->{capture};
                if ($quote_flags->{l}) {
                    $ast = Perlito5::AST::Apply->new( namespace => '', code => 'lcfirst', arguments => [$ast] );
                    delete $quote_flags->{l};
                }
                if ($quote_flags->{u}) {
                    $ast = Perlito5::AST::Apply->new( namespace => '', code => 'ucfirst', arguments => [$ast] );
                    delete $quote_flags->{u};
                }
                $ast = Perlito5::AST::Apply->new( namespace => '', code => 'lc', arguments => [$ast] )
                    if $quote_flags->{L};
                $ast = Perlito5::AST::Apply->new( namespace => '', code => 'uc', arguments => [$ast] )
                    if $quote_flags->{U};
                $ast = Perlito5::AST::Apply->new( namespace => '', code => 'quotemeta', arguments => [$ast] )
                    if $quote_flags->{Q};
                $match->{capture} = $ast;
            }
            $m = $match;
        }
        elsif ($c eq '\\') {
            if ($interpolate) {
                # \L \U \Q .. \E - lowercase/uppercase/quotemeta until /E or end-of-string
                # \l \u          - lowercase/uppercase 1 char
                if ($c2 eq 'E') {
                    my $flag_to_reset = $quote_flags->{last_flag};
                    if ($flag_to_reset) {
                        delete($quote_flags->{$flag_to_reset});
                        delete($quote_flags->{last_flag});
                    }
                    else {
                        $quote_flags = {}
                    }
                    $p += 1;
                    $c = ''
                }
                elsif ($c2 eq 'L') {
                    $quote_flags->{$c2} = 1;
                    delete $quote_flags->{U};
                    $quote_flags->{last_flag} = $c2;
                    $p += 1;
                    $c = ''
                }
                elsif ($c2 eq 'U') {
                    $quote_flags->{$c2} = 1;
                    delete $quote_flags->{L};
                    $quote_flags->{last_flag} = $c2;
                    $p += 1;
                    $c = ''
                }
                elsif ($c2 eq 'Q') {
                    $quote_flags->{$c2} = 1;
                    $quote_flags->{last_flag} = $c2;
                    $p += 1;
                    $c = ''
                }
                elsif ($c2 eq 'l') {
                    $quote_flags->{$c2} = 1
                        unless $quote_flags->{u};
                    $p += 1;
                    $c = ''
                }
                elsif ($c2 eq 'u') {
                    $quote_flags->{$c2} = 1
                        unless $quote_flags->{l};
                    $p += 1;
                    $c = ''
                }
            }
            if ($c) {
                if ($interpolate == 2) {
                    # regex
                    $m = { str => $str, from => $p, to => $p+2, capture => Perlito5::AST::Buf->new( buf => $str->[$p] . $str->[$p+1] ) }
                }
                elsif ($interpolate == 1) {
                    # double-quotes
                    $m = Perlito5::Grammar::String::double_quoted_unescape( $str, $p );
                }
                else {
                    # single-quotes
                    $m =  $c2 eq "\\"
                        ? { str => $str, from => $p, to => $p+2, capture => Perlito5::AST::Buf->new( buf => "\\" ) }
                        : $c2 eq "'"
                        ? { str => $str, from => $p, to => $p+2, capture => Perlito5::AST::Buf->new( buf => "'" ) }
                        : 0;
                }
            }
        }
        if ( $m ) {
            my $obj = Perlito5::Match::flat($m);
            if ( ref($obj) eq 'Perlito5::AST::Buf' ) {
                $buf .= apply_quote_flags($obj->{'buf'}, $quote_flags);
                $obj = undef;
            }
            if ( $obj ) {
                if ( length $buf ) {
                    push @args, Perlito5::AST::Buf->new( buf => $buf );
                    $buf = '';
                }
                push @args, $obj;
            }
            $p = $m->{to};
            $buf .= $more;
        }
        else {
            $p++;
            if ( $c eq chr(10) || $c eq chr(13) ) {
                # after a newline, check for here-docs
                my $m = here_doc( $str, $p );
                if ( $p != $m->{to} ) {
                    $p = $m->{to};
                }
                else {
                    $buf .= apply_quote_flags($c, $quote_flags);
                }
            }
            else {
                $buf .= apply_quote_flags($c, $quote_flags);
            }
        }
    }
    if ( length $buf ) {
        push @args, Perlito5::AST::Buf->new( buf => $buf );
    }

    Perlito5::Compiler::error "Can't find string terminator '$delimiter' anywhere before EOF"
        if join('', @{$str}[ $p .. $p + length($delimiter) - 1]) ne $delimiter;

    $p += length($delimiter);

    my $ast;
    if (!@args) {
        $ast = Perlito5::AST::Buf->new( buf => '' )
    }
    elsif (@args == 1 && ref($args[0]) eq 'Perlito5::AST::Buf') {
        $ast = $args[0];
    }
    else {
        unshift @args, Perlito5::AST::Buf->new( buf => '' )
            if @args == 1;

        $ast = Perlito5::AST::Apply->new(
            namespace => '',
            code => 'list:<.>',
            arguments => \@args,
            Perlito5::overloading_flag(),
        )
    }
    
    return {
        'str' => $str, 
        'from' => $pos, 
        'to' => $p, 
        capture => $ast
    };
}


our @Here_doc;
sub here_doc_wanted {
    # setup a here-doc request
    # the actual text will be parsed later, by here_doc()

    my $str = $_[0];
    my $pos = $_[1];    # $pos points to the first "<" in <<'END'

    my $delimiter;
    my $type = 'double_quote';
    my $indented = 0;
    my $p = $pos;
    if ( $str->[$p] eq '<' && $str->[$p + 1] eq '<' ) {
        # <<
        $p += 2;
        my $quote = $str->[$p];
        if ( $quote eq "~" ) {
            $indented = 1;
            $p++;
            $quote = $str->[$p];
        }
        if ( $quote eq "'" || $quote eq '"' ) {
            $p += 1;
            my $m = string_interpolation_parse($_[0], $p, $quote, $quote, 0);
            if ( $m ) {
                $p = $m->{to};
                $delimiter = Perlito5::Match::flat($m)->{buf};
                $type = $quote eq "'" ? 'single_quote' : 'double_quote';
                # say "got a $type here-doc delimiter: [$delimiter]";
            }
        }
        else {
            $p += 1 if $quote eq '\\';
            my $m = Perlito5::Grammar::ident($str, $p);
            if ( $m ) {
                $p = $m->{to};
                $delimiter = Perlito5::Match::flat($m);
                $type = $quote eq '\\' ? 'single_quote' : 'double_quote';
                # say "got a $type here-doc delimiter: [$delimiter]";
            }
            else {
                warn 'Use of bare << to mean <<"" is deprecated'
                    if $Perlito5::WARNINGS;
                $delimiter = '';
                $type = 'double_quote';
            }
        }
    }

    if ( !defined $delimiter ) {
        # not a here-doc request, return false
        return 0;
    }

    my $placeholder = Perlito5::AST::Apply->new(
        code      => 'list:<.>',
        namespace => '',
        arguments => [

            # XXX - test 12 t/base/lex.t fails if we don't use this "double-pointer"

            Perlito5::AST::Apply->new(
                code      => 'list:<.>',
                namespace => '',
                arguments => []
              )
        ]
    );

    push @Here_doc, [
        $type,
        $placeholder->{arguments}[0]{arguments},
        $delimiter,
        $indented,
    ];

    return {
        'str' => $str,
        'from' => $pos,
        'to' => $p,
        capture => [
                'term',
                $placeholder
            ]
    };
}

token newline {
    | \c10 \c13?
    | \c13 \c10?
};


sub here_doc {
    # here-doc is called just after a newline

    my $str = $_[0];
    my $pos = $_[1];

    if ( !@Here_doc ) {
        # we are not expecting a here-doc, return true without moving the pointer
        return {
            'str' => $str, 'from' => $pos, 'to' => $pos
        };
    }
    my $p = $pos;
    my $here = shift @Here_doc;
    my $type      = $here->[0];
    my $result    = $here->[1];
    my $delimiter = $here->[2];
    my $indented  = $here->[3];     # TODO indented heredoc
    # say "got a newline and we are looking for a $type that ends with ", $delimiter;
    while ( $p < @$str ) {
        my $spaces = '';
        my $p0 = $p;
        if ($indented) {
            while ( $p < @$str ) {
                last if $str->[$p] ne ' ' && $str->[$p] ne "\t";
                $p++;
            }
            $spaces = join('', @{$str}[ $p0 .. $p - 1 ] );
        }
        if ( join('', @{$str}[ $p .. $p + length($delimiter) - 1]) eq $delimiter ) {
            my $c = $str->[ $p + length($delimiter) ];
            if ($c eq '' || $c eq " " || $c eq "\t" || $c eq chr(10) || $c eq chr(13) ) {
                # this will put the text in the right place in the AST

                # unindent the heredoc by $spaces
                my @here_string = split( "\n", join('', @{$str}[ $pos .. $p - 1]), -1 );
                if (length($spaces)) {
                    my $l = length($spaces);
                    for my $i (0 .. $#here_string) {
                        if ( substr($here_string[$i], 0, $l) eq $spaces ) {
                            $here_string[$i] = substr( $here_string[$i], $l );
                        }
                        else {
                            Perlito5::Compiler::error "Indentation on line $i of here-doc doesn't match delimiter";
                        }
                    }
                }

                if ($type eq 'single_quote') {
                    # single_quote
                    # TODO - single quote escapes like \' and \\
                    push @$result, Perlito5::AST::Buf->new(buf => join("\n", @here_string));
                }
                else {
                    # double_quote
                    my $m;
                    my $str = [ split "", join("\n", @here_string, $delimiter . "\n") ];
                    $m = string_interpolation_parse($str, 0, '', "\n" . $delimiter . "\n", 1);
                    if ( $m ) {
                        push @$result, Perlito5::Match::flat($m);
                    }
                    else {
                        Perlito5::Compiler::error 'Can\'t find string terminator "' . $delimiter . '" anywhere before EOF';
                    }
                }

                $p += length($delimiter);
                # say "$p ", scalar(@$str);
                my $m = newline( $str, $p );
                if ( $p >= @$str || $m ) {
                    # return true
                    $p = $m->{to} if $m;
                    return {
                        'str' => $str, 'from' => $pos, 'to' => $p - 1
                    }
                }
            }
        }
        # ... next line
        while (  $p < @$str
              && ( $str->[$p] ne chr(10) && $str->[$p] ne chr(13) )
              )
        {
            $p++
        }
        while (  $p < @$str
              && ( $str->[$p] eq chr(10) || $str->[$p] eq chr(13) )
              )
        {
            $p++
        }
    }
    Perlito5::Compiler::error 'Can\'t find string terminator "' . $delimiter . '" anywhere before EOF';
}


sub double_quoted_unescape {
    my $str = $_[0];
    my $pos = $_[1];

    my $c2 = $str->[$pos+1];
    my $m;
    if ( exists $escape_sequence{$c2} ) {
        $m = {
            'str' => $str,
            'from' => $pos,
            'to' => $pos+2,
            capture => Perlito5::AST::Buf->new( buf => chr($escape_sequence{$c2}) ),
        };
    }
    elsif ( $c2 eq 'c' ) {
        # \cC = Control-C
        # \c0 = "p"
        my $c3 = ord(uc( $str->[$pos+2] )) - ord('A') + 1;
        $c3 = 128 + $c3 
            if $c3 < 0;
        $m = {
            'str' => $str,
            'from' => $pos,
            'to' => $pos+3,
            capture => Perlito5::AST::Buf->new( buf => chr($c3) ),
        };
    }
    elsif ( $c2 eq 'x' ) {
        if ($str->[$pos+2] eq '{') {
            # \x{03a3}         - unicode hex
            my $p = $pos+3;
            $p++
                while $p < @$str && $str->[$p] ne '}';
            my $hex_code = join("", @{$str}[ $pos+3 .. $p - 1 ]);
            $hex_code = "0" unless $hex_code;
            my $tmp = oct( "0x" . $hex_code );
            $m = {
                str => $str,
                from => $pos,
                to => $p + 1,
                capture => Perlito5::AST::Buf->new( buf => chr($tmp) ),
            };
        }
        else {
            # "\x"+hex  - max 2 digit hex
            my $p = $pos+2;
            $p++ if $hex{ uc $str->[$p] };
            $p++ if $hex{ uc $str->[$p] };
            my $hex_code = join("", @{$str}[ $pos+2 .. $p - 1 ]);
            $hex_code = "0" unless $hex_code;
            my $tmp = oct( "0x" . $hex_code );
            $m = {
                str => $str,
                from => $pos,
                to => $p,
                capture => Perlito5::AST::Buf->new( buf => chr($tmp) ),
            };
        }
    }
    elsif ( exists $octal{$c2} ) {
        # "\"+octal        - initial zero is optional; max 3 digit octal (377)
        my $p = $pos+1;
        $p++ if $octal{ $str->[$p] };
        $p++ if $octal{ $str->[$p] };
        $p++ if $octal{ $str->[$p] };
        my $oct_code = join("", @{$str}[ $pos+1 .. $p - 1 ]);
        my $tmp = oct($oct_code);
        $m = {
            str => $str,
            from => $pos,
            to => $p,
            capture => Perlito5::AST::Buf->new( buf => chr($tmp) ),
        };
    }
    elsif ( $c2 eq 'N' ) {
        if ($str->[$pos+2] eq '{') {
            if ($str->[$pos+3] eq 'U' && $str->[$pos+4] eq '+') {
                #  \N{U+263D}  Unicode character
                my $p = $pos+5;
                $p++
                    while $p < @$str && $str->[$p] ne '}';
                my $hex_code = join("", @{$str}[ $pos+5 .. $p - 1 ]);
                $hex_code = "0" unless $hex_code;
                my $tmp = oct( "0x" . $hex_code );
                $m = {
                    str => $str,
                    from => $pos,
                    to => $p + 1,
                    capture => Perlito5::AST::Buf->new( buf => chr($tmp) ),
                };
            }
            else {
                #  \N{name}    named Unicode character
                my $p = $pos+3;
                $p++
                    while $p < @$str && $str->[$p] ne '}';
                my $name = join("", @{$str}[ $pos+3 .. $p - 1 ]);
                my ($hex_code) = get_unicode_table() =~ /\n([0-9A-Z]+);$name;/;
                Perlito5::Compiler::error "Unknown charname '$name'"
                    if !$hex_code;
                my $tmp = oct( "0x" . $hex_code );
                $m = {
                    str => $str,
                    from => $pos,
                    to => $p + 1,
                    capture => Perlito5::AST::Buf->new( buf => chr($tmp) ),
                };
            }
        }
    }
    else {
        $m = {
            'str' => $str,
            'from' => $pos,
            'to' => $pos+2,
            capture => Perlito5::AST::Buf->new( buf => $c2 ),
        };
    }
    return $m;
}

sub double_quoted_var_with_subscript {
    my $m_var = $_[0];
    my $interpolate = $_[1];  # 0 - single-quote; 1 - double-quote; 2 - regex

    my $str = $m_var->{str};
    my $pos = $m_var->{to};
    my $p = $pos;
    my $m_index;
    if ($str->[$p] eq '-' && $str->[$p+1] eq '>' && $str->[$p+2] eq '[') {
        # ->[
        $p += 3;
        $m_index = Perlito5::Grammar::Expression::list_parse($str, $p);
        Perlito5::Compiler::error "syntax error" unless $m_index;
        my $exp = $m_index->{capture};
        $p = $m_index->{to};
        Perlito5::Compiler::error "syntax error" if $exp eq '*undef*' || $str->[$p] ne ']';
        $p++;
        $m_index->{capture} = Perlito5::AST::Call->new(
                method    => 'postcircumfix:<[ ]>',
                invocant  => $m_var->{capture},
                arguments => $exp,
            );
        $m_index->{to} = $p;
        return double_quoted_var_with_subscript($m_index, $interpolate);
    }
    if ($str->[$p] eq '-' && $str->[$p+1] eq '>' && $str->[$p+2] eq '{') {
        # '->{'
        $pos += 2;
        $m_index = Perlito5::Grammar::Expression::term_curly($str, $pos);
        Perlito5::Compiler::error "syntax error" unless $m_index;
        $m_index->{capture} = Perlito5::AST::Call->new(
                method    => 'postcircumfix:<{ }>',
                invocant  => $m_var->{capture},
                arguments => Perlito5::Match::flat($m_index)->[2][0],
            );
        return double_quoted_var_with_subscript($m_index, $interpolate);
    }
    if ($str->[$p] eq '[') {

        if ($interpolate == 2) {
            # inside a regex: disambiguate from char-class
            # these are valid indexes: 12 -1 $x
            my $m = Perlito5::Grammar::Number::term_digit($str, $p+1)
                 || (  $str->[$p+1] eq '-'
                    && Perlito5::Grammar::Number::term_digit($str, $p+2)
                    )
                 || Perlito5::Grammar::Sigil::term_sigil($str, $p+1);
            return $m_var unless $m;
            return $m_var unless $str->[$m->{to}] eq ']';
        }

        $p++;
        $m_index = Perlito5::Grammar::Expression::list_parse($str, $p);
        if ($m_index) {
            my $exp = $m_index->{capture};
            $p = $m_index->{to};
            if ($exp ne '*undef*' && $str->[$p] eq ']') {
                $p++;
                my $value = $m_var->{capture};
                if (ref($value) eq 'Perlito5::AST::Var') {
                    $value->{_real_sigil} = '@';
                }
                $m_index->{capture} = Perlito5::AST::Index->new(
                        obj       => $value,
                        index_exp => $exp,
                    );
                $m_index->{to} = $p;
                return double_quoted_var_with_subscript($m_index, $interpolate);
            }
        }
    }
    $m_index = Perlito5::Grammar::Expression::term_curly($str, $pos);
    if ($m_index) {
        my $value = $m_var->{capture};
        if (ref($value) eq 'Perlito5::AST::Var') {
            $value->{_real_sigil} = '%';
        }
        $m_index->{capture} = Perlito5::AST::Lookup->new(
                obj       => $value,
                index_exp => Perlito5::Match::flat($m_index)->[2][0],
            );
        return double_quoted_var_with_subscript($m_index, $interpolate);
    }

    return $m_var;
}

sub double_quoted_var {
    my $str = $_[0];
    my $pos = $_[1];
    my $delimiter = $_[2];
    my $interpolate = $_[3];  # 0 - single-quote; 1 - double-quote; 2 - regex

    my $c = $str->[$pos];
    my $c2 = $str->[$pos+1];

    if ($c eq '$' && $c2 eq ')') {
        return 0;
    }
    elsif ($c eq '$' && $c2 eq '{') {
        my $m = Perlito5::Grammar::Sigil::term_sigil($str, $pos);
        return $m unless $m;
        my $var = Perlito5::Match::flat($m)->[1];
        $m->{capture} = $var;
        return $m;
    }
    elsif ($c eq '$'
          && $c2 eq '$'
          && !Perlito5::Grammar::word($str, $pos+2)
          )
    {
        # "$$a" -> ${$a}
        # "$$1" -> ${$1}
        # "$$_" -> ${$_}
        # "$$." -> $$ . "."
        return {
            str => $str,
            capture => Perlito5::AST::Var->new( name => '$', sigil => '$', namespace => '' ),  # special var $$
            from => $pos,
            to => $pos+2,
        };
    }
    elsif ($c eq '$'
          && join('', @{$str}[ $pos+1 .. $pos + length($delimiter) ]) ne $delimiter)
    {
        # TODO - this only covers simple expressions
        # TODO - syntax errors are allowed here - this should backtrack

        my $m = Perlito5::Grammar::Sigil::term_sigil($str, $pos);
        return $m unless $m;

        $m->{capture} = $m->{capture}[1];
        return double_quoted_var_with_subscript($m, $interpolate);
    }
    elsif ($c eq '@'
        && join('', @{$str}[ $pos+1 .. $pos + length($delimiter) ]) ne $delimiter  # not end of string
        && ($c2 lt "0" || $c2 gt "9")                                   # not a digit
        )
    {
        my $m = Perlito5::Grammar::Sigil::term_sigil($str, $pos);
        return $m unless $m;

        $m->{capture} = $m->{capture}[1];
        $m = double_quoted_var_with_subscript($m, $interpolate);

        $m->{capture} = 
             Perlito5::AST::Apply->new(
                code      => 'join',
                arguments => [
                        Perlito5::AST::Var->new( name => '"', sigil => '$', namespace => '' ),  # special var $"
                        $m->{capture}
                    ],
                namespace => ''
             );
        return $m;
    }
    return 0;
}


Perlito5::Grammar::Precedence::add_term( "'"  => \&term_q_quote );
Perlito5::Grammar::Precedence::add_term( '"'  => \&term_qq_quote );
Perlito5::Grammar::Precedence::add_term( '/'  => \&term_m_quote );
Perlito5::Grammar::Precedence::add_term( '<'  => \&term_glob );
Perlito5::Grammar::Precedence::add_term( '<<>>' => \&term_glob );
Perlito5::Grammar::Precedence::add_term( '<<' => \&here_doc_wanted );
Perlito5::Grammar::Precedence::add_term( '`'  => \&term_qx );
Perlito5::Grammar::Precedence::add_term( 'm'  => \&term_m_quote );
Perlito5::Grammar::Precedence::add_term( 'q'  => \&term_q_quote );
Perlito5::Grammar::Precedence::add_term( 'qq' => \&term_qq_quote );
Perlito5::Grammar::Precedence::add_term( 'qw' => \&term_qw_quote );
Perlito5::Grammar::Precedence::add_term( 'qx' => \&term_qx );
Perlito5::Grammar::Precedence::add_term( 'qr' => \&term_qr_quote );
Perlito5::Grammar::Precedence::add_term( 's'  => \&term_s_quote );
Perlito5::Grammar::Precedence::add_term( 'tr' => \&term_tr_quote );
Perlito5::Grammar::Precedence::add_term( 'y'  => \&term_tr_quote );


1;

