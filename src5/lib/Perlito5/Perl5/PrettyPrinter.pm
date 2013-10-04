package Perlito5::Perl5::PrettyPrinter;
use strict;
use warnings;

my %dispatch = (
    stmt    => \&statement,                 # if (expr) {stms}
    block   => \&block,                     # {stmts}
    keyword => \&keyword,                   # if
    bareword => \&bareword,                 # main
    op      => \&op,                        # expr
    paren   => \&paren,                     # (expr)
    paren_semicolon => \&paren_semicolon,   # (expr;expr;expr)
    comment => \&comment,                   # # comment
);

my %pair = (
    '(' => ')',
    '[' => ']',
    '{' => '}',
);

our %op = (
    'prefix:<-->' => { fix => 'prefix', prec => 1, str => '--' },
    'prefix:<++>' => { fix => 'prefix', prec => 1, str => '++' },
    'postfix:<-->' => { fix => 'postfix', prec => 1, str => '--' },
    'postfix:<-->' => { fix => 'postfix', prec => 1, str => '++' },

    'infix:<**>' => { fix => 'infix', prec => 2, str => '**' },

    'prefix:<\\>' => { fix => 'prefix', prec => 3, str => '\\' },
    'prefix:<+>' => { fix => 'prefix', prec => 3, str => '+' },
    'prefix:<->' => { fix => 'prefix', prec => 3, str => '-' },
    'prefix:<~>' => { fix => 'prefix', prec => 3, str => '~' },
    'prefix:<!>' => { fix => 'prefix', prec => 3, str => '!' },

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

    # TODO - named unary, -X
    'prefix:<do>' => { fix => 'prefix', prec => 8, str => 'do ' },
    'prefix:<-f>' => { fix => 'prefix', prec => 8, str => '-f ' },

    'infix:<lt>' => { fix => 'infix', prec => 9, str => ' lt ' },  
    'infix:<le>' => { fix => 'infix', prec => 9, str => ' le ' },   
    'infix:<gt>' => { fix => 'infix', prec => 9, str => ' gt ' },   
    'infix:<ge>' => { fix => 'infix', prec => 9, str => ' ge ' },   
    'infix:<<=>' => { fix => 'infix', prec => 9, str => ' <= ' },   
    'infix:<>=>' => { fix => 'infix', prec => 9, str => ' >= ' },   
    'infix:<<>' =>  { fix => 'infix', prec => 9, str => ' < ' },   
    'infix:<>>' =>  { fix => 'infix', prec => 9, str => ' > ' },   

    # TODO - more operators

    'infix:<=>'  => { fix => 'infix',  prec => 19, str => ' = ' },

    'infix:<=>>'  => { fix => 'infix',  prec => 20, str => ' => ' },
    'list:<,>'  => { fix => 'list',  prec => 20, str => ', ' },

    # TODO - more operators

);

my %tab;

sub tab {
    my $level = $_[0];
    $tab{$level} //= "    " x $level;
}

sub op_precedence {
    my ( $data ) = @_;
    return 0 if !ref($data);
    return 0 if $data->[0] ne 'op';
    return $op{ $data->[1] }{prec} || 0;
}

sub statement_need_semicolon {
    my ( $data ) = @_;
    return 1 if !ref($data);
    return 0 if $data->[0] eq 'block';
    return 0 if $data->[0] eq 'comment';
    return 1 if $data->[0] ne 'stmt';   # stmt => [ keyword => 'if' ],
    if (ref($data->[1])) {
        my $dd = $data->[1];            # [ keyword => 'if' ],
        if ($dd->[0] eq 'keyword') {
            return 0
                if $dd->[1] eq 'if' || $dd->[1] eq 'for' || $dd->[1] eq 'while';
        }
    }
    return 1;
}

sub op_render {
    my ( $data, $level, $out, $current_op ) = @_;
    if ( ref($data) ) {
        my $this_prec = op_precedence($data);
        push @$out, '(' if $current_op->{prec} < $this_prec;
        $dispatch{ $data->[0] }->( $data, $level, $out );
        push @$out, ')' if $current_op->{prec} < $this_prec;
    }
    else {
        push @$out, $data;
    }
}

sub op {
    my ( $data, $level, $out ) = @_;
    my $op = $data->[1];
    my $spec = $op{$op} || die "unknown op: $op";
    if ($spec->{fix} eq 'infix') {
        op_render( $data->[2], $level, $out, $spec );
        push @$out, $spec->{str};
        op_render( $data->[3], $level, $out, $spec );
    }
    elsif ($spec->{fix} eq 'prefix') {
        push @$out, $spec->{str};
        op_render( $data->[2], $level, $out, $spec );
    }
    elsif ($spec->{fix} eq 'postfix') {
        op_render( $data->[2], $level, $out, $spec );
        push @$out, $spec->{str};
    }
    elsif ($spec->{fix} eq 'list') {
        for my $line ( 2 .. $#$data ) {
            op_render( $data->[$line], $level, $out, $spec );
            push @$out, $spec->{str} if $line != $#$data;
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
        if ($line != $#$data) {
            if (@{ $data->[$line+1] }) {
                push @$out, '; ';
            }
            else {
                push @$out, ';';
            }
        }
    }
    push @$out, $pair{$data->[1]};
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

sub block {
    my ( $data, $level, $out ) = @_;
    if ( @$data == 1 ) {
        push @$out, "{}";
        return;
    }
    push @$out, '{', "\n";
    $level++;
    for my $line ( 1 .. $#$data ) {
        my $d    = $data->[$line];
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
        my $d    = $data->[$line];
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

