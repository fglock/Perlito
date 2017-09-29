package Perlito5::Perl5::PrettyPrinter;
use strict;
use warnings;

# my %dispatch = (
#     stmt            => \&statement,             # if (expr) {stms}
#     stmt_modifier   => \&statement_modifier,    # stmt if expr
#     block           => \&block,                 # {stmts}
#     keyword         => \&keyword,               # if
#     bareword        => \&bareword,              # main
#     number          => \&number,                # number
#     op              => \&op,                    # expr
#     paren           => \&paren,                 # (expr)
#     paren_semicolon => \&paren_semicolon,       # (expr;expr;expr)
#     apply           => \&apply,                 # subr(expr)
#     call            => \&call,                  # expr->subr(expr)
#     comment         => \&comment,               # # comment
#     label           => \&label,                 # L1:
# );

# XXX - TODO - workaround initialization order in the javascript backend
my %dispatch = (
    stmt            => sub { statement(@_) },             # if (expr) {stms}
    stmt_modifier   => sub { statement_modifier(@_) },    # stmt if expr
    block           => sub { block(@_) },                 # {stmts}
    keyword         => sub { keyword(@_) },               # if
    bareword        => sub { bareword(@_) },              # main
    number          => sub { number(@_) },                # number
    op              => sub { op(@_) },                    # expr
    paren           => sub { paren(@_) },                 # (expr)
    paren_semicolon => sub { paren_semicolon(@_) },       # (expr;expr;expr)
    apply           => sub { apply(@_) },                 # subr(expr)
    call            => sub { call(@_) },                  # expr->subr(expr)
    comment         => sub { comment(@_) },               # # comment
    label           => sub { label(@_) },                 # L1:
);

my %pair = (
    '(' => ')',
    '[' => ']',
    '{' => '}',
    '<' => '>',
);

our %op = (
    'prefix:<$>'  => { fix => 'deref', prec => 0, str => '$'  }, 
    'prefix:<@>'  => { fix => 'deref', prec => 0, str => '@'  },
    'prefix:<%>'  => { fix => 'deref', prec => 0, str => '%'  },
    'prefix:<&>'  => { fix => 'deref', prec => 0, str => '&'  },
    'prefix:<*>'  => { fix => 'deref', prec => 0, str => '*'  },
    'prefix:<$#>' => { fix => 'deref', prec => 0, str => '$#' }, 

    'circumfix:<[ ]>' => { fix => 'circumfix',  prec => 0, str => '[' },
    'circumfix:<{ }>' => { fix => 'circumfix',  prec => 0, str => '{' },
    'circumfix:<( )>' => { fix => 'circumfix',  prec => 0, str => '(' },

    'infix:<->>' => { fix => 'infix', prec => -1, str => '->' },

    'prefix:<-->'  => { fix => 'prefix',  prec => 1, str => '--' },
    'prefix:<++>'  => { fix => 'prefix',  prec => 1, str => '++' },
    'postfix:<-->' => { fix => 'postfix', prec => 1, str => '--' },
    'postfix:<++>' => { fix => 'postfix', prec => 1, str => '++' },

    'infix:<**>' => { fix => 'infix', prec => 2, str => '**' },

    'prefix:<\\>' => { fix => 'prefix', prec => 3, str => '\\' },
    'prefix:<+>'  => { fix => 'prefix', prec => 3, str => '+' },
    'prefix:<->'  => { fix => 'prefix', prec => 3, str => '-' },
    'prefix:<~>'  => { fix => 'prefix', prec => 3, str => '~' },
    'prefix:<!>'  => { fix => 'prefix', prec => 3, str => '!' },

    'infix:<=~>' => { fix => 'infix', prec => 4, str => ' =~ ' },
    'infix:<!~>' => { fix => 'infix', prec => 4, str => ' !~ ' },

    'infix:<*>' => { fix => 'infix', prec => 5, str => ' * ' },
    'infix:</>' => { fix => 'infix', prec => 5, str => ' / ' },
    'infix:<%>' => { fix => 'infix', prec => 5, str => ' % ' },
    'infix:<x>' => { fix => 'infix', prec => 5, str => ' x ' },

    'infix:<+>' => { fix => 'infix', prec => 6, str => ' + ' },
    'infix:<->' => { fix => 'infix', prec => 6, str => ' - ' },
    'list:<.>'  => { fix => 'list',  prec => 6, str => ' . ' },

    'infix:<<<>' => { fix => 'infix', prec => 7, str => ' << ' },
    'infix:<>>>' => { fix => 'infix', prec => 7, str => ' >> ' },

    # named unary - see below

    'infix:<lt>' => { fix => 'infix', prec => 9, str => ' lt ' },
    'infix:<le>' => { fix => 'infix', prec => 9, str => ' le ' },
    'infix:<gt>' => { fix => 'infix', prec => 9, str => ' gt ' },
    'infix:<ge>' => { fix => 'infix', prec => 9, str => ' ge ' },
    'infix:<<=>' => { fix => 'infix', prec => 9, str => ' <= ' },
    'infix:<>=>' => { fix => 'infix', prec => 9, str => ' >= ' },
    'infix:<<>'  => { fix => 'infix', prec => 9, str => ' < ' },
    'infix:<>>'  => { fix => 'infix', prec => 9, str => ' > ' },

    'infix:<<=>>' => { fix => 'infix', prec => 10, str => ' <=> ' },
    'infix:<cmp>' => { fix => 'infix', prec => 10, str => ' cmp ' },
    'infix:<==>'  => { fix => 'infix', prec => 10, str => ' == ' },
    'infix:<!=>'  => { fix => 'infix', prec => 10, str => ' != ' },
    'infix:<ne>'  => { fix => 'infix', prec => 10, str => ' ne ' },
    'infix:<eq>'  => { fix => 'infix', prec => 10, str => ' eq ' },

    'infix:<&>' => { fix => 'infix', prec => 11, str => ' & ' },

    'infix:<|>' => { fix => 'infix', prec => 12, str => ' | ' },
    'infix:<^>' => { fix => 'infix', prec => 12, str => ' ^ ' },

    'infix:<..>'  => { fix => 'infix', prec => 13, str => ' .. ' },
    'infix:<...>' => { fix => 'infix', prec => 13, str => ' ... ' },
    'infix:<~~>'  => { fix => 'infix', prec => 13, str => ' ~~ ' },

    'infix:<&&>' => { fix => 'infix', prec => 14, str => ' && ' },

    'infix:<||>' => { fix => 'infix', prec => 15, str => ' || ' },
    'infix:<//>' => { fix => 'infix', prec => 15, str => ' // ' },

    'ternary:<? :>' => { fix => 'ternary', prec => 16 },

    'infix:<=>'   => { fix => 'infix', prec => 17, str => ' = ' },
    'infix:<**=>' => { fix => 'infix', prec => 17, str => ' **= ' },
    'infix:<+=>'  => { fix => 'infix', prec => 17, str => ' += ' },
    'infix:<-=>'  => { fix => 'infix', prec => 17, str => ' -= ' },
    'infix:<*=>'  => { fix => 'infix', prec => 17, str => ' *= ' },
    'infix:</=>'  => { fix => 'infix', prec => 17, str => ' /= ' },
    'infix:<x=>'  => { fix => 'infix', prec => 17, str => ' x= ' },
    'infix:<|=>'  => { fix => 'infix', prec => 17, str => ' |= ' },
    'infix:<&=>'  => { fix => 'infix', prec => 17, str => ' &= ' },
    'infix:<.=>'  => { fix => 'infix', prec => 17, str => ' .= ' },
    'infix:<<<=>' => { fix => 'infix', prec => 17, str => ' <<= ' },
    'infix:<>>=>' => { fix => 'infix', prec => 17, str => ' >>= ' },
    'infix:<%=>'  => { fix => 'infix', prec => 17, str => ' %= ' },
    'infix:<||=>' => { fix => 'infix', prec => 17, str => ' ||= ' },
    'infix:<&&=>' => { fix => 'infix', prec => 17, str => ' &&= ' },
    'infix:<^=>'  => { fix => 'infix', prec => 17, str => ' ^= ' },
    'infix:<//=>' => { fix => 'infix', prec => 17, str => ' //= ' },

    'prefix:<next>' => { fix => 'prefix', prec => 17, str => 'next ' },
    'prefix:<last>' => { fix => 'prefix', prec => 17, str => 'last ' },
    'prefix:<redo>' => { fix => 'prefix', prec => 17, str => 'redo ' },

    'infix:<=>>' => { fix => 'infix', prec => 18, str => ' => ' },

    'list:<,>' => { fix => 'list', prec => 19, str => ', ' },

    'prefix:<not>' => { fix => 'prefix', prec => 20, str => 'not ' },

    'infix:<and>' => { fix => 'infix', prec => 21, str => ' and ' },

    'infix:<or>'  => { fix => 'infix', prec => 22, str => ' or ' },
    'infix:<xor>' => { fix => 'infix', prec => 22, str => ' xor ' },
);

# more unary operators
$op{ "prefix:<$_>" } = { fix => 'prefix', prec => 8, str => "$_ " }
    for qw(
        -r -w -x -o
        -R -W -X -O
        -e -z -s
        -f -d -l -p -S -b -c -t
        -u -g -k
        -T -B
        -M -A -C
    );
$op{ "prefix:<$_>" } = { fix => 'parsed', prec => 15, str => "$_" }
    for qw( do sub my our state local
            eval map grep sort print use );

my %tab;
sub tab {
    my $level = $_[0];
    $tab{$level} //= "    " x $level;
}

sub render {
    my ( $data, $level, $out ) = @_;
    if ( ref($data) ) {
        $dispatch{ $data->[0] }->( $data, $level, $out );
    }
    else {
        push @$out, $data;
    }
}

sub op_precedence {
    my ($data) = @_;
    return 0 if !ref($data);
    return 0 if $data->[0] ne 'op';
    return $op{ $data->[1] }{prec} || 0;
}

sub statement_need_semicolon {
    my ($data) = @_;
    return 1 if !ref($data);
    return 0
        if $data->[0] eq 'block' || $data->[0] eq 'comment' || $data->[0] eq 'label';
    if ( $data->[0] eq 'stmt' ) {
        # stmt => [ keyword => 'if' ],
        if ( ref( $data->[1] ) ) {
            my $dd = $data->[1];    # [ keyword => 'if' ],
            if ( $dd->[0] eq 'keyword' ) {
                return 0
                  if ref( $data->[-1] ) && $data->[-1][0] eq 'block';
            }
        }
    }
    return 1;
}

sub op_render {
    my ( $data, $level, $out, $current_op ) = @_;
    if ( ref($data) ) {
        my $this_prec = op_precedence($data);
        push @$out, '(' if $this_prec && $current_op->{prec} && $current_op->{prec} < $this_prec;
        render( $data, $level, $out );
        push @$out, ')' if $this_prec && $current_op->{prec} && $current_op->{prec} < $this_prec;
    }
    else {
        push @$out, $data;
    }
}

sub op {
    my ( $data, $level, $out ) = @_;
    my $op = $data->[1];
    my $spec = $op{$op} || die "unknown op: $op";
    if ( $spec->{fix} eq 'infix' ) {
        op_render( $data->[2], $level, $out, $spec );
        push @$out, $spec->{str};
        op_render( $data->[3], $level, $out, $spec );
    }
    elsif ( $spec->{fix} eq 'prefix' ) {
        push @$out, $spec->{str};
        if ($op eq 'prefix:<not>' && !$data->[2]) {
            # not() needs parenthesis
            push @$out, '()';
        }
        else {
            op_render( $data->[2], $level, $out, $spec );
        }
    }
    elsif ( $spec->{fix} eq 'postfix' ) {
        op_render( $data->[2], $level, $out, $spec );
        push @$out, $spec->{str};
    }
    elsif ( $spec->{fix} eq 'ternary' ) {
        op_render( $data->[2], $level, $out, $spec );
        push @$out, ' ? ';
        op_render( $data->[3], $level, $out, $spec );
        push @$out, ' : ';
        op_render( $data->[4], $level, $out, $spec );
    }
    elsif ( $spec->{fix} eq 'deref' ) {
        # push @$out, $spec->{str}, '{';
        push @$out, $spec->{str};
        op_render( $data->[2], $level, $out, $spec );
        # push @$out, '}';
    }
    elsif ( $spec->{fix} eq 'circumfix' ) {
        push @$out, $spec->{str};
        for my $line ( 2 .. $#$data ) {
            op_render( $data->[$line], $level, $out, $spec );
            push @$out, ', ' if $line != $#$data;
        }
        push @$out, $pair{$spec->{str}};
    }
    elsif ( $spec->{fix} eq 'list' ) {
        for my $line ( 2 .. $#$data ) {
            op_render( $data->[$line], $level, $out, $spec );
            push @$out, $spec->{str} if $line != $#$data;
        }
    }
    elsif ( $spec->{fix} eq 'parsed' ) {
        push @$out, $spec->{str};
        for my $line ( 2 .. $#$data ) {
            my $d = $data->[$line];
            push @$out, ' ';
            render( $d, $level, $out );
        }
    }
    else {
        die "unknown fixity: $spec->{fix}";
    }
    return;
}

sub call {
    my ( $data, $level, $out ) = @_;
    my @dd = @$data;
    shift @dd;
    my $open = '(';
    render( shift(@dd), $level, $out );
    push @$out, '->';
    my $d = $dd[0];
    render( $d, $level, $out );
    $dd[0] = 'list:<,>';
    push @$out, $open;
    op( [ op => @dd ], $level, $out );
    push @$out, $pair{$open};
}

sub apply {
    my ( $data, $level, $out ) = @_;
    my @dd = @$data;
    shift @dd;
    my $open = shift @dd;
    my $d = $dd[0];
    if ($d eq 'return') {
        # 'return' should not have 'function' parentheses, because 
        #    return 4,5     # '5' in scalar context
        #    return (4,5)   # '2' in scalar context
        $open = ' ';
    }
    render( $d, $level, $out );
    $dd[0] = 'list:<,>';
    push @$out, $open;
    op( [ op => @dd ], $level, $out );
    push @$out, $pair{$open};
}

sub paren {
    my ( $data, $level, $out ) = @_;
    my @dd = @$data;
    shift @dd;
    my $open = $dd[0];
    $dd[0] = 'list:<,>';
    push @$out, $open;
    op( [ op => @dd ], $level, $out );
    push @$out, $pair{$open};
}

sub paren_semicolon {
    my ( $data, $level, $out ) = @_;
    push @$out, $data->[1];
    for my $line ( 2 .. $#$data ) {
        render( $data->[$line], $level, $out ) if @{ $data->[$line] };
        if ( $line != $#$data ) {
            if ( @{ $data->[ $line + 1 ] } ) {
                push @$out, '; ';
            }
            else {
                push @$out, ';';
            }
        }
    }
    push @$out, $pair{ $data->[1] };
}

sub label {
    my ( $data, $level, $out ) = @_;
    push @$out, $data->[1], ":";
    return;
}

sub keyword {
    my ( $data, $level, $out ) = @_;
    push @$out, $data->[1];
    return;
}

sub bareword {
    my ( $data, $level, $out ) = @_;
    push @$out, $data->[1];
    return;
}

sub number {
    my ( $data, $level, $out ) = @_;
    push @$out, $data->[1];
    return;
}

sub comment {
    my ( $data, $level, $out ) = @_;
    push @$out, $data->[1];
    return;
}

sub statement {
    my ( $data, $level, $out ) = @_;
    for my $line ( 1 .. $#$data ) {
        my $d = $data->[$line];
        render( $d, $level, $out );
        push @$out, ' ' if $line != $#$data;
    }
}

sub statement_modifier {
    my ( $data, $level, $out ) = @_;
    render( $data->[1], $level, $out );
    push @$out, "\n", tab($level + 1);
    render( $data->[2], $level, $out );
}

sub block {
    my ( $data, $level, $out ) = @_;
    if ( @$data == 1 ) {
        push @$out, "{}";
        return;
    }
    if ( @$data == 2 ) {
        push @$out, '{;', "\n";     # disambiguate { $x, $y } block vs. hash
    }
    else {
        push @$out, '{', "\n";
    }
    $level++;
    for my $line ( 1 .. $#$data ) {
        my $d = $data->[$line];
        push @$out, tab($level);
        render( $d, $level, $out );
        push @$out, ';' if $line != $#$data && statement_need_semicolon($d);
        push @$out, "\n";
    }
    $level--;
    push @$out, tab($level), '}';
}

sub pretty_print {
    my ( $data, $level, $out ) = @_;
    for my $line ( 0 .. $#$data ) {
        my $d = $data->[$line];
        push @$out, tab($level);
        render( $d, $level, $out );
        push @$out, ';' if $line != $#$data && statement_need_semicolon($d);
        push @$out, "\n";
    }
}

1;

__END__

    my $data = [
        'block',
        [ stmt => [ keyword => 'if' ], [ paren => '(', '@a' ], ['block', [ 'stmt' => '123' ]] ],
        [ block => [ 'stmt' => '2' ], [ 'stmt' => '3' ], ],
        ['block'],
        [ op => 'list:<,>', '2', '3', [ 'op', 'infix:<*>', ['op', 'infix:<+>', 4, 7], ['op', 'infix:<**>', 5, 2] ] ],
        123,
        [ stmt => [ keyword => 'for' ], [ paren_semicolon => '(', [], [], [] ], ['block'] ],
        [ stmt => [ keyword => 'for' ], [ paren_semicolon => '(', ['op', 'infix:<=>', '$i', 0], [], [] ], ['block'] ],
    ];
    my $out = [];
    Perlito5::Perl5::PrettyPrinter::block( $data, 0, $out );
    print join( '', @$out ), "\n";

