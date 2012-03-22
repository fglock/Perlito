use v5;

package Perlito5::Grammar::String;

use Perlito5::Precedence;

Perlito5::Precedence::add_term( "'"  => sub { Perlito5::Grammar::String->term_single_quote($_[0], $_[1]) } );
Perlito5::Precedence::add_term( '"'  => sub { Perlito5::Grammar::String->term_double_quote($_[0], $_[1]) } );
Perlito5::Precedence::add_term( '<<' => sub { Perlito5::Grammar::String->here_doc_wanted($_[0], $_[1]) } );
Perlito5::Precedence::add_term( 'q'  => sub { Perlito5::Grammar::String->term_q_quote($_[0], $_[1]) } );
Perlito5::Precedence::add_term( 'qq' => sub { Perlito5::Grammar::String->term_qq_quote($_[0], $_[1]) } );
Perlito5::Precedence::add_term( 'qw' => sub { Perlito5::Grammar::String->term_qw_quote($_[0], $_[1]) } );
Perlito5::Precedence::add_term( '/'  => sub { Perlito5::Grammar::String->term_slash_quote($_[0], $_[1]) } );
Perlito5::Precedence::add_term( 'm'  => sub { Perlito5::Grammar::String->term_m_quote($_[0], $_[1]) } );
Perlito5::Precedence::add_term( 's'  => sub { Perlito5::Grammar::String->term_s_quote($_[0], $_[1]) } );

token term_double_quote {
    \" <double_quote_parse>
        { $MATCH->{"capture"} = [ 'term', $MATCH->{"double_quote_parse"}->flat() ]  }
};
token term_single_quote {
    \' <single_quote_parse>
        { $MATCH->{"capture"} = [ 'term', $MATCH->{"single_quote_parse"}->flat() ]  }
};
token term_q_quote {
    'q' [ '#' | <.Perlito5::Grammar.opt_ws> <!before <.Perlito5::Grammar.word> > <char_any> ] <q_quote_parse>
        { $MATCH->{"capture"} = [ 'term', $MATCH->{"q_quote_parse"}->flat() ]  }
};
token term_qq_quote {
    'qq' [ '#' | <.Perlito5::Grammar.opt_ws> <!before <.Perlito5::Grammar.word> > <char_any> ] <qq_quote_parse>
        { $MATCH->{"capture"} = [ 'term', $MATCH->{"qq_quote_parse"}->flat() ]  }
};
token term_qw_quote {
    'qw' [ '#' | <.Perlito5::Grammar.opt_ws> <!before <.Perlito5::Grammar.word> > <char_any> ] <qw_quote_parse>
        { $MATCH->{"capture"} = [ 'term', $MATCH->{"qw_quote_parse"}->flat() ]  }
};
token term_slash_quote {
    '/' <m_quote_parse>
        { 
            $MATCH->{"capture"} = [ 'term', $MATCH->{"m_quote_parse"}->flat() ]  
        }
};
token term_m_quote {
    'm' [ '#' | <.Perlito5::Grammar.opt_ws> <!before <.Perlito5::Grammar.word> > <char_any> ] <m_quote_parse>
        {
            $MATCH->{"capture"} = [ 'term', $MATCH->{"m_quote_parse"}->flat() ]  
        }
};
token term_s_quote {
    's' [ '#' | <.Perlito5::Grammar.opt_ws> <!before <.Perlito5::Grammar.word> > <char_any> ] <s_quote_parse>
        { 
            $MATCH->{"capture"} = [ 'term', $MATCH->{"s_quote_parse"}->flat() ]  
        }
};


my %pair = (
    '{' => '}',
    '(' => ')',
    '[' => ']',
    '<' => '>',
);

sub q_quote_parse {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
    my $delimiter = substr( $str, $pos-1, 1 );
    my $open_delimiter = $delimiter;
    $delimiter = $pair{$delimiter} if exists $pair{$delimiter};
    return $self->string_interpolation_parse($str, $pos, $open_delimiter, $delimiter, 0);
}
sub qq_quote_parse {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
    my $delimiter = substr( $str, $pos-1, 1 );
    my $open_delimiter = $delimiter;
    $delimiter = $pair{$delimiter} if exists $pair{$delimiter};
    return $self->string_interpolation_parse($str, $pos, $open_delimiter, $delimiter, 1);
}
sub qw_quote_parse {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
    my $delimiter = substr( $str, $pos-1, 1 );
    my $open_delimiter = $delimiter;
    $delimiter = $pair{$delimiter} if exists $pair{$delimiter};

    my $m = $self->string_interpolation_parse($str, $pos, $open_delimiter, $delimiter, 0);
    if ( $m ) {
        $m->{"capture"} = Perlito5::AST::Apply->new(
                code      => 'list:<,>',
                arguments => [ map( Perlito5::AST::Val::Buf->new( buf => $_ ), split(' ', $m->flat()->{"buf"})) ],
                namespace => '',
            );
    }
    return $m;
}
sub m_quote_parse {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
    my $delimiter = substr( $str, $pos-1, 1 );
    my $open_delimiter = $delimiter;
    my $closing_delimiter = $delimiter;
    $closing_delimiter = $pair{$delimiter} if exists $pair{$delimiter};
    my $part1 = $self->string_interpolation_parse($str, $pos, $open_delimiter, $closing_delimiter, 1);
    return $part1 unless $part1;

    # TODO - call the regex compiler
    my $str_regex = Perlito5::AST::Val::Buf->new( buf => substr( $str, $pos, $part1->{"to"} - $pos - 1 ) );

    my $m;
    my $p = $part1->{"to"};

    my $modifiers = '';
    $m = Perlito5::Grammar->ident($str, $p);
    if ( $m ) {
        $modifiers = $m->flat();
        $part1->{"to"} = $m->{"to"};
    }

    $part1->{"capture"} = Perlito5::AST::Apply->new( 
        code => 'p5:m',
        arguments => [ $str_regex, $modifiers ],
        namespace => ''
    );
    return $part1;
}
sub s_quote_parse {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
    my $delimiter = substr( $str, $pos-1, 1 );
    my $open_delimiter = $delimiter;
    my $closing_delimiter = $delimiter;
    $closing_delimiter = $pair{$delimiter} if exists $pair{$delimiter};
    my $part1 = $self->string_interpolation_parse($str, $pos, $open_delimiter, $closing_delimiter, 1);
    return $part1 unless $part1;

    # TODO - call the regex compiler
    my $str_regex = Perlito5::AST::Val::Buf->new( buf => substr( $str, $pos, $part1->{"to"} - $pos - 1 ) );

    my $part2;
    my $m;
    my $p = $part1->{"to"};
    if ( exists $pair{$delimiter} ) {
        # warn "pair delimiter $delimiter at $p";
        $m = Perlito5::Grammar->opt_ws($str, $p);
        $p = $m->{"to"};
        $delimiter = substr( $str, $p, 1 );
        my $open_delimiter = $delimiter;
        $p++;
        # warn "second delimiter $delimiter";
        $closing_delimiter = $delimiter;
        $closing_delimiter = $pair{$delimiter} if exists $pair{$delimiter};
        $part2 = $self->string_interpolation_parse($str, $p, $open_delimiter, $closing_delimiter, 1);
        return $part2 unless $part2;
    }
    else {
        $part2 = $self->string_interpolation_parse($str, $p, $open_delimiter, $closing_delimiter, 1);
        return $part2 unless $part2;
    }

    $p = $part2->{"to"};
    my $modifiers = '';
    $m = Perlito5::Grammar->ident($str, $p);
    if ( $m ) {
        $modifiers = $m->flat();
        $part2->{"to"} = $m->{"to"};
    }

    $part2->{"capture"} = Perlito5::AST::Apply->new( 
        code => 'p5:s',
        arguments => [ $str_regex, $part2->flat(), $modifiers ],
        namespace => ''
    );
    return $part2;
}

sub string_interpolation_parse {
    my $self           = $_[0];
    my $str            = $_[1];
    my $pos            = $_[2];
    my $open_delimiter = $_[3];
    my $delimiter      = $_[4];
    my $interpolate    = $_[5];

    my $p = $pos;

    my $balanced = $open_delimiter && exists $pair{$open_delimiter};

    my @args;
    my $buf = '';
    while (  $p < length($str)
          && substr($str, $p, length($delimiter)) ne $delimiter
          )
    {
        my $c = substr($str, $p, 1);
        my $m;
        my $more = '';
        if ($balanced && $c eq $open_delimiter) {
            $buf .= $c;
            $p++;
            $m = $self->string_interpolation_parse($str, $p, $open_delimiter, $delimiter, $interpolate);
            $more = $delimiter;
        }
        else {
            $m = $interpolate
                ? Perlito5::Grammar::String->double_quoted_buf( $str, $p, $delimiter )
                : Perlito5::Grammar::String->single_quoted_unescape( $str, $p );
        }
        if ( $m ) {
            my $obj = $m->flat();
            if ( ref($obj) eq 'Perlito5::AST::Val::Buf' ) {
                $buf .= $obj->{"buf"};
                $obj = undef;
            }
            if ( $obj ) {
                if ( length $buf ) {
                    push @args, Perlito5::AST::Val::Buf->new( buf => $buf );
                    $buf = '';
                }
                push @args, $obj;
            }
            $p = $m->{"to"};
            $buf .= $more;
        }
        else {
            $buf .= $c;
            $p++;
            if ( $c eq chr(10) || $c eq chr(13) ) {
                # after a newline, check for here-docs
                my $m = $self->here_doc( $str, $p );
                $p = $m->{"to"};
            }
        }
    }
    if ( length $buf ) {
        push @args, Perlito5::AST::Val::Buf->new( buf => $buf );
    }

    die "Can't find string terminator '$delimiter' anywhere before EOF"
        if substr($str, $p, length($delimiter)) ne $delimiter;

    $p += length($delimiter);

    my $ast;
    if (!@args) {
        $ast = Perlito5::AST::Val::Buf->new( buf => '' )
    }
    elsif (@args == 1) {
        $ast = $args[0];
    }
    else {
        $ast = Perlito5::AST::Apply->new(
            namespace => '',
            code => 'list:<.>',
            arguments => \@args,
        )
    }
    
    return Perlito5::Match->new(
        'str' => $str, 
        'from' => $pos, 
        'to' => $p, 
        capture => $ast
    );
}


my @Here_doc;
sub here_doc_wanted {
    # setup a here-doc request
    # the actual text will be parsed later, by here_doc()

    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];    # $pos points to the first "<" in <<'END'

    my $delimiter;
    my $type = 'double_quote';
    my $p = $pos;
    if ( substr($str, $p, 2) eq '<<' ) {
        $p += 2;
        my $quote = substr($str, $p, 1);
        if ( $quote eq "'" || $quote eq '"' ) {
            $p += 1;
            my $m = $self->string_interpolation_parse($str, $p, $quote, $quote, 0);
            if ( $m ) {
                $p = $m->{"to"};
                $delimiter = $m->flat()->{"buf"};
                $type = $quote eq "'" ? 'single_quote' : 'double_quote';
                # say "got a $type here-doc delimiter: [$delimiter]";
            }
        }
        else {
            $p += 1 if $quote eq '\\';
            my $m = Perlito5::Grammar->ident($str, $p);
            if ( $m ) {
                $p = $m->{"to"};
                $delimiter = $m->flat();
                $type = $quote eq '\\' ? 'single_quote' : 'double_quote';
                # say "got a $type here-doc delimiter: [$delimiter]";
            }
            else {
                die 'Use of bare << to mean <<"" is deprecated';
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
        arguments => [] 
    );
    my $placeholder2 = Perlito5::AST::Apply->new( 
        code      => 'list:<.>',
        namespace => '',
        arguments => [$placeholder] 
    );
    push @Here_doc, [
        $type,
        sub { $placeholder->{"arguments"} = $_[0] },
        $delimiter,
    ];

    return Perlito5::Match->new(
        'str' => $str,
        'from' => $pos,
        'to' => $p,
        capture => [
                'term',
                $placeholder2
            ]
    );
}

token newline {
    | \c10 \c13?
    | \c13 \c10?
};


sub here_doc {
    # here-doc is called just after a newline

    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];

    if ( !@Here_doc ) {
        # we are not expecting a here-doc, return true without moving the pointer
        return Perlito5::Match->new(
            'str' => $str, 'from' => $pos, 'to' => $pos, capture => undef);
    }

    my $p = $pos;
    my $here = shift @Here_doc;
    my $type      = $here->[0];
    my $delimiter = $here->[2];
    # say "got a newline and we are looking for a $type that ends with ", $delimiter;
    if ($type eq 'single_quote') {
        while ( $p < length($str) ) {
            if ( substr($str, $p, length($delimiter)) eq $delimiter ) {
                # this will put the text in the right place in the AST
                $here->[1]->( [Perlito5::AST::Val::Buf->new(buf => substr($str, $pos, $p - $pos))] );
                $p += length($delimiter);
                # say "$p ", length($str);
                my $m = $self->newline( $str, $p );
                if ( $p >= length($str) || $m ) {
                    # return true
                    $p = $m->{"to"} if $m;
                    return Perlito5::Match->new(
                        'str' => $str, 'from' => $pos, 'to' => $p - 1, capture => undef);
                }
            }
            # ... next line
            while (  $p < length($str)
                  && ( substr($str, $p, 1) ne chr(10) && substr($str, $p, 1) ne chr(13) )
                  )
            {
                $p++
            }
            while (  $p < length($str)
                  && ( substr($str, $p, 1) eq chr(10) || substr($str, $p, 1) eq chr(13) )
                  )
            {
                $p++
            }
        }
    }
    else {
        # double_quote
        my $m;
        if ( substr($str, $p, length($delimiter)) eq $delimiter ) {
            $p += length($delimiter);
            $m = $self->newline( $str, $p );
            if ( $p >= length($str) || $m ) {
                $here->[1]->( [Perlito5::AST::Val::Buf->new( buf => '' )] );
                $p = $m->{"to"} if $m;
                return Perlito5::Match->new(
                    'str' => $str, 'from' => $pos, 'to' => $p, capture => undef);
            }
        }

        # TODO - compare to newline() instead of "\n"

        $m = $self->string_interpolation_parse($str, $pos, '', "\n" . $delimiter . "\n", 1);
        if ( $m ) {
            $here->[1]->( [
                    $m->flat(), 
                    Perlito5::AST::Val::Buf->new( buf => "\n" ),
                ] );
            return $m;
        }
    }
    die 'Can\'t find string terminator "' . $delimiter . '" anywhere before EOF';
}


sub single_quote_parse {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
    return $self->string_interpolation_parse($str, $pos, "'", "'", 0);
}

sub double_quote_parse {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
    return $self->string_interpolation_parse($str, $pos, '"', '"', 1);
}
 

token char_any {
    .
};

token single_quoted_unescape {
    |  \\ \\
        { $MATCH->{"capture"} = Perlito5::AST::Val::Buf->new( buf => "\\" ) }
    |  \\ \'
        { $MATCH->{"capture"} = Perlito5::AST::Val::Buf->new( buf => '\'' ) }
};

sub double_quoted_buf {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
    my $delimiter = $_[3];

    if  (  (substr($str, $pos, 1) eq '$' || substr($str, $pos, 1) eq '@')
        && substr($str, $pos+1, length($delimiter)) ne $delimiter
        )
    {
        # TODO - this only covers simple expressions
        # TODO - syntax errors are allowed here - this should backtrack

        my $m = Perlito5::Expression->term_sigil($str, $pos);
        return $m unless $m;

        my $var = $m->flat()->[1];

        my $p = $m->{"to"};
        my $m_index;
        if (substr($str, $p, 1) eq '[') {
            $p++;
            $m_index = Perlito5::Expression->list_parse($str, $p);
            if ($m_index) {
                my $exp = $m_index->{"capture"}{"exp"};
                $p = $m_index->{"to"};
                if ($exp ne '*undef*' && substr($str, $p, 1) eq ']') {
                    $p++;
                    $m_index->{"capture"} = Perlito5::AST::Index->new(
                            obj       => $var,
                            index_exp => $exp,
                        );
                    $m_index->{"to"} = $p;
                    return $m_index;

                }
            }
        }
        $m_index = Perlito5::Expression->term_curly($str, $m->{"to"});
        if ($m_index) {
            $m_index->{"capture"} = Perlito5::AST::Lookup->new(
                    obj       => $var,
                    index_exp => $m_index->flat()->[2][0],
                );
            return $m_index;
        }

        if (substr($str, $pos, 1) eq '@') {
            $m->{"capture"} = 
                 Perlito5::AST::Apply->new(
                    code      => 'join',
                    arguments => [ Perlito5::AST::Val::Buf->new( buf => ' ' ), $var ],
                    namespace => ''
                 );
        }
        else {
            $m->{"capture"} = $var;
        }
        return $m;
    }
    elsif (substr($str, $pos, 1) eq '\\') {
        my $m = $self->double_quoted_unescape($str, $pos+1);
        $m->{"capture"} = Perlito5::AST::Val::Buf->new( buf => $m->flat() );
        return $m;
    }
    return 0;
}

token double_quoted_unescape {

    # TODO - "\"+octal "\x"+hex  - initial zero is optional; max 3 digit octal (377); max 2 digit hex
    # TODO - \cC = Control-C
    # TODO - \N{charname}     - requires "use charnames"
    # TODO - \x{03a3}         - unicode hex
    # TODO - \L \Q \U ... \E  - lowercase/uppercase/quote until /E
    # TODO - \l \u            - uppercase next char

    ## |  c
    ##     [   \[ <Perlito5::Grammar.digits> \]
    ##         { $MATCH->{"capture"} = chr( $MATCH->{"Perlito5::Grammar.digits"}->flat() ) }
    ##     |  <Perlito5::Grammar.digits>
    ##         { $MATCH->{"capture"} = chr( $MATCH->{"Perlito5::Grammar.digits"}->flat() ) }
    ##     ]

    |  a
        { $MATCH->{"capture"} = chr(7) }
    |  b
        { $MATCH->{"capture"} = chr(8) }
    |  e
        { $MATCH->{"capture"} = chr(27) }
    |  f
        { $MATCH->{"capture"} = chr(12) }
    |  n
        { $MATCH->{"capture"} = chr(10) }
    |  r
        { $MATCH->{"capture"} = chr(13) }
    |  t
        { $MATCH->{"capture"} = chr(9) }
    |  <char_any>
        { $MATCH->{"capture"} = $MATCH->{"char_any"}->flat() }
};

1;

