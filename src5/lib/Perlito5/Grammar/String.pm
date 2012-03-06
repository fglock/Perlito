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
            $buf .= substr($str, $p, 1);
            $p++;
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

