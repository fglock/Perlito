package Perlito5::Perl5::PrettyPrinter;
use strict;
use warnings;

my %dispatch = (
    stmt            => \&statement,             # if (expr) {stms}
    stmt_modifier   => \&statement_modifier,    # stmt if expr
    block           => \&block,                 # {stmts}
    keyword         => \&keyword,               # if
    bareword        => \&bareword,              # main
    op              => \&op,                    # expr
    paren           => \&paren,                 # (expr)
    paren_semicolon => \&paren_semicolon,       # (expr;expr;expr)
    comment         => \&comment,               # # comment
);

my %pair = (
    '(' => ')',
    '[' => ']',
    '{' => '}',
);

our %op = (
    'circumfix:<[ ]>' => { fix => 'circumfix',  prec => 0, str => '[' },
    'circumfix:<{ }>' => { fix => 'circumfix',  prec => 0, str => '{' },
    'circumfix:<( )>' => { fix => 'circumfix',  prec => 0, str => '(' },

    'prefix:<-->'  => { fix => 'prefix',  prec => 1, str => '--' },
    'prefix:<++>'  => { fix => 'prefix',  prec => 1, str => '++' },
    'postfix:<-->' => { fix => 'postfix', prec => 1, str => '--' },
    'postfix:<-->' => { fix => 'postfix', prec => 1, str => '++' },

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

    # TODO - more named unary
    'prefix:<-f>'    => { fix => 'prefix', prec => 8, str => '-f ' },
    'prefix:<do>'    => { fix => 'parsed', prec => 8, str => 'do ' },
    'prefix:<sub>'   => { fix => 'parsed', prec => 8, str => 'sub' },
    'prefix:<my>'    => { fix => 'parsed', prec => 8, str => 'my' },
    'prefix:<our>'   => { fix => 'parsed', prec => 8, str => 'our' },
    'prefix:<state>' => { fix => 'parsed', prec => 8, str => 'state' },

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

    'infix:<=>>' => { fix => 'infix', prec => 18, str => ' => ' },

    'list:<,>' => { fix => 'list', prec => 19, str => ', ' },

    'prefix:<not>' => { fix => 'infix', prec => 20, str => ' not ' },

    'infix:<and>' => { fix => 'infix', prec => 21, str => ' and ' },

    'infix:<or>'  => { fix => 'infix', prec => 22, str => ' or ' },
    'infix:<xor>' => { fix => 'infix', prec => 22, str => ' xor ' },
);

my %tab;

sub tab {
    my $level = $_[0];
    $tab{$level} //= "    " x $level;
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
    return 0 if $data->[0] eq 'block';
    return 0 if $data->[0] eq 'comment';
    if ( $data->[0] eq 'stmt' ) {

        # stmt => [ keyword => 'if' ],
        if ( ref( $data->[1] ) ) {
            my $dd = $data->[1];    # [ keyword => 'if' ],
            if ( $dd->[0] eq 'keyword' ) {
                return 0
                  if ref( $data->[-1] ) && $data->[-1][0] eq 'block';

                # if $dd->[1] eq 'if' || $dd->[1] eq 'for' || $dd->[1] eq 'while';
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
        $dispatch{ $data->[0] }->( $data, $level, $out );
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
        op_render( $data->[2], $level, $out, $spec );
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
    elsif ( $spec->{fix} eq 'circumfix' ) {
        push @$out, $spec->{str};
        op_render( $data->[2], $level, $out, $spec );
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
            if ( ref($d) ) {
                $dispatch{ $d->[0] }->( $d, $level, $out );
            }
            else {
                push @$out, $d;
            }
        }
    }
    else {
        die "unknown fixity: $spec->{fix}";
    }
    return;
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
        op( $data->[$line], $level, $out ) if @{ $data->[$line] };
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

sub comment {
    my ( $data, $level, $out ) = @_;
    push @$out, $data->[1];
    return;
}

sub statement {
    my ( $data, $level, $out ) = @_;
    for my $line ( 1 .. $#$data ) {
        my $d = $data->[$line];
        if ( ref($d) ) {
            $dispatch{ $d->[0] }->( $d, $level, $out );
        }
        else {
            push @$out, $d;
        }
        push @$out, ' ' if $line != $#$data;
    }
}

sub statement_modifier {
    my ( $data, $level, $out ) = @_;
    for my $line ( 1 .. 2 ) {
        my $d = $data->[$line];
        push @$out, tab($level);
        if ( ref($d) ) {
            $dispatch{ $d->[0] }->( $d, $level, $out );
        }
        else {
            push @$out, $d;
        }
        push @$out, "\n" if $line == 1;
        $level++;
    }
}

sub block {
    my ( $data, $level, $out ) = @_;
    if ( @$data == 1 ) {
        push @$out, "{}";
        return;
    }
    push @$out, '{', "\n";
    $level++;
    for my $line ( 1 .. $#$data ) {
        my $d = $data->[$line];
        push @$out, tab($level);
        if ( ref($d) ) {
            $dispatch{ $d->[0] }->( $d, $level, $out );
        }
        else {
            push @$out, $d;
        }
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
        if ( ref($d) ) {
            $dispatch{ $d->[0] }->( $d, $level, $out );
        }
        else {
            push @$out, $d;
        }
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

