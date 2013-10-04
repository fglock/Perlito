use strict;
use warnings;

package Perlito5::PrettyPrint {

    use Data::Dumper;

    my %dispatch = (
        stmt    => \&statement,
        block   => \&block,
        keyword => \&keyword,
        op      => \&op,
        paren   => \&paren,
    );

    my %op = (
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

        'infix:<+>' => { fix => 'infix', prec => 6, str => ' + ' },

        'list:<,>'  => { fix => 'list',  prec => 9, str => ', ' },
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
        return 1 if $data->[0] ne 'stmt';
        if (ref($data->[1])) {
            my $dd = $data->[1];    # stmt => [ keyword => 'if' ],
            if ($dd->[0] eq 'keyword') {
                if ($dd->[1] eq 'if' || $dd->[1] eq 'for' || $dd->[1] eq 'while') {
                    return 0;
                }
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
        my $cmd = $data->[0];
        my $op = $data->[1];
        my $spec = $op{$op} || {};
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
            die "unknown op: $op";
        }
        return;
    }

    sub paren {
        my ( $data, $level, $out ) = @_;
        my @dd = @$data;
        $dd[0] = 'list:<,>';
        push @$out, '(';
        op( [ op => @dd ], $level, $out );
        push @$out, ')';
    }

    sub keyword {
        my ( $data, $level, $out ) = @_;
        push @$out, $data->[1];
        return;
    }

    sub statement {
        my ( $data, $level, $out ) = @_;
        my $cmd = $data->[0];
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
        my @dd = @$data;
        my $cmd = $dd[0];
        if ( @dd == 1 ) {
            push @$out, "{}";
            return;
        }
        push @$out, '{', "\n";
        $level++;
        for my $line ( 1 .. $#dd ) {
            my $d    = $dd[$line];
            my $out1 = [];
            if ( ref($d) ) {
                $dispatch{ $d->[0] }->( $d, $level, $out1 );
            }
            else {
                push @$out1, $d;
            }
            push @$out, tab($level), @$out1;
            push @$out, ';' if $line != $#dd && statement_need_semicolon($d);
            push @$out, "\n";
        }
        $level--;
        push @$out, tab($level), '}';
    }

}

{
    my $data = [
        'block',
        [ stmt => [ keyword => 'if' ], [ paren => '@a' ], ['block', [ 'stmt' => '123' ]] ],
        [ block => [ 'stmt' => '2' ], [ 'stmt' => '3' ], ],
        ['block'],
        [ op => 'list:<,>', '2', '3', [ 'op', 'infix:<*>', ['op', 'infix:<+>', 4, 7], ['op', 'infix:<**>', 5, 2] ] ],
        123,
    ];
    my $out = [];
    Perlito5::PrettyPrint::block( $data, 0, $out );
    print join( '', @$out ), "\n";
}

__END__

