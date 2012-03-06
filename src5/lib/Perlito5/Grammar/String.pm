use v5;

package Perlito5::Grammar::String;

sub string_interpolation_parse {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
    my $delimiter = $_[3];
    my $interpolate = $_[4];

    my $p = $pos;

    my @args;
    my $buf = '';
    while (  $p < length($str)
          && substr($str, $p, length($delimiter)) ne $delimiter
          )
    {
        my $m = $interpolate
                ? Perlito5::Grammar::String->double_quoted_buf( $str, $p )
                : Perlito5::Grammar::String->single_quoted_unescape( $str, $p );
        if ( $m->{"bool"} ) {
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
        }
        else {
            my $c = substr($str, $p, 1);
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
        'bool' => 1, 
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
    my $p = $pos;
    if ( substr($str, $p, 2) eq '<<' ) {
        $p += 2;
        if ( substr($str, $p, 1) eq "'" ) {
            $p += 1;
            my $m = Perlito5::Grammar::String->single_quote_parse( $str, $p );
            if ( $m->{"bool"} ) {
                $p = $m->{"to"};
                $delimiter = $m->flat()->{"buf"};
                # say "got a here-doc delimiter: [$delimiter]";
            }
        }
    }

    if ( !defined $delimiter ) {
        # not a here-doc request, return false
        return Perlito5::Match->new(
            'str' => $str, 'from' => $pos, 'to' => $pos, 'bool' => 0, capture => undef);
    }

    my $placeholder = Perlito5::AST::Val::Buf->new( buf => 'HEREDOC' );
    push @Here_doc, [
        'single_quote',
        sub { $placeholder->{"buf"} = $_[0] },
        $delimiter,
    ];

    return Perlito5::Match->new(
        'str' => $str,
        'from' => $pos,
        'to' => $p,
        'bool' => 1,
        capture => [
                'term',
                $placeholder
            ]
    );
}

token newline {
    | \c10 \c13?
    | \c13 \c10?
}


sub here_doc {
    # here-doc is called just after a newline

    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];

    if ( !@Here_doc ) {
        # we are not expecting a here-doc, return true without moving the pointer
        return Perlito5::Match->new(
            'str' => $str, 'from' => $pos, 'to' => $pos, 'bool' => 1, capture => undef);
    }

    my $p = $pos;
    my $here = shift @Here_doc;
    my $delimiter = $here->[2];
    # say "got a newline and we are looking for a ", $here->[0], " that ends with ", $delimiter;
    while ( $p < length($str) ) {
        if ( substr($str, $p, length($delimiter)) eq $delimiter ) {
            # this will put the text in the right place in the AST
            $here->[1]->(substr($str, $pos, $p - $pos));
            $p += length($delimiter);
            # say "$p ", length($str);
            my $m = $self->newline( $str, $p );
            if ( $p >= length($str) || $m->{"bool"} ) {
                # return true
                $p = $m->{"to"} if $m->{"bool"};
                return Perlito5::Match->new(
                    'str' => $str, 'from' => $pos, 'to' => $p, 'bool' => 1, capture => undef);
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
    die 'Can\'t find string terminator "' . $delimiter . '" anywhere before EOF';
}


sub single_quote_parse {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
    return $self->string_interpolation_parse($str, $pos, "'", 0);
}

sub double_quote_parse {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
    return $self->string_interpolation_parse($str, $pos, '"', 1);
}
 

token char_any {
    .
}

token single_quoted_unescape {
    |  \\ \\
        { $MATCH->{"capture"} = Perlito5::AST::Val::Buf->new( buf => "\\" ) }
    |  \\ \'
        { $MATCH->{"capture"} = Perlito5::AST::Val::Buf->new( buf => '\'' ) }
}

token double_quoted_unescape {
    |  c
        [   \[ <Perlito5::Grammar.digits> \]
            { $MATCH->{"capture"} = chr( $MATCH->{"Perlito5::Grammar.digits"}->flat() ) }
        |  <Perlito5::Grammar.digits>
            { $MATCH->{"capture"} = chr( $MATCH->{"Perlito5::Grammar.digits"}->flat() ) }
        ]
    |  e
        { $MATCH->{"capture"} = chr(27) }
    |  n
        { $MATCH->{"capture"} = "\n" }
    |  t
        { $MATCH->{"capture"} = chr(9) }
    |  <char_any>
        { $MATCH->{"capture"} = $MATCH->{"char_any"}->flat() }
}

token double_quoted_buf {
    | <before \$ >
        [ <Perlito5::Expression.term_sigil>
            { $MATCH->{"capture"} = $MATCH->{"Perlito5::Expression.term_sigil"}->flat()->[1] }
        | <char_any>
            { $MATCH->{"capture"} = Perlito5::AST::Val::Buf->new( buf => $MATCH->{"char_any"}->flat() ) }
        ]
    | <before \@ >
        [ <Perlito5::Expression.term_sigil>
            { $MATCH->{"capture"} = Perlito5::AST::Apply->new(
                    namespace => '',
                    code      => 'join',
                    arguments => [
                        Perlito5::AST::Val::Buf->new( buf => ' ' ),
                        ($MATCH->{"Perlito5::Expression.term_sigil"}->flat())[1]
                    ],
                )
            }
        | <char_any>
            { $MATCH->{"capture"} = Perlito5::AST::Val::Buf->new( buf => $MATCH->{"char_any"}->flat() ) }
        ]
    | \\ <double_quoted_unescape>
        { $MATCH->{"capture"} = Perlito5::AST::Val::Buf->new( buf => $MATCH->{"double_quoted_unescape"}->flat() ) }
}

token val_buf {
    | \" <double_quote_parse>
        { $MATCH->{"capture"} = $MATCH->{"double_quote_parse"}->flat() }
    | \' <single_quote_parse>
        { $MATCH->{"capture"} = $MATCH->{"single_quote_parse"}->flat() }
}

1;

