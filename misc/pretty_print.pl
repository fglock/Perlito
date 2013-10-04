use strict;

package Perlito5::PrettyPrint {

    use Data::Dumper;

    my %dispatch = (
        stmt    => \&statement,
        block   => \&block,
        list    => \&list,
        keyword => \&keyword,
        op      => \&op,
    );

    my %op = (
        'infix:<*>' => { fix => 'infix', prec => 1, str => '*' },
        'infix:<+>' => { fix => 'infix', prec => 2, str => '+' },
    );

    # TODO
    # precedence
    # prefix/infix/postfix

    my %tab;

    sub tab {
        my $level = $_[0];
        $tab{$level} //= "\t" x $level;
    }

    sub op {
        my ( $data, $level, $out ) = @_;
        my $cmd = $data->[0];
        my $op = $data->[1];
        my $spec = $op{$op} || {};
        if ($spec->{fix} eq 'infix') {
            push @$out, $data->[2], ' ', $spec->{str}, ' ', $data->[3];
        }
        else {
            die "unknown op: $op";
        }
        return;
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
            push @$out, ';' if $line != $#dd;
            push @$out, "\n";
        }
        $level--;
        push @$out, tab($level), '}';
    }

    sub list {
        my ( $data, $level, $out ) = @_;
        my @dd = @$data;
        my $cmd = $dd[0];
        my $op = $dd[1];
        push @$out, '(';
        $level++;
        for my $line ( 2 .. $#dd ) {
            my $d    = $dd[$line];
            my $out1 = [];
            if ( ref($d) ) {
                $dispatch{ $d->[0] }->( $d, $level, $out1 );
            }
            else {
                push @$out1, $d;
            }
            push @$out, @$out1;
            push @$out, $op if $line != $#dd;
        }
        $level--;
        push @$out, ')';
    }

}

{
    my $data = [
        'block',
        [ 'stmt', [ keyword => 'if' ] ],
        [ 'block', [ 'stmt' => '2' ], [ 'stmt' => '3' ], ],
        ['block'],
        [ 'list', ', ', '2', '3', [ 'op', 'infix:<+>', 4, 5 ] ],
    ];
    my $out = [];
    Perlito5::PrettyPrint::block( $data, 0, $out );
    print join( '', @$out );
}

__END__

 $out =                                         "try {"
     . $tab                                   .    join($tab, @str) . "\n"
     . Perlito5::Javascript2::tab($level)     . '}' . "\n"
     . Perlito5::Javascript2::tab($level)     . 'catch(err) {' . "\n"
     . Perlito5::Javascript2::tab($level + 1) .    'if ( err instanceof Error ) {' . "\n"
     . Perlito5::Javascript2::tab($level + 2)         . 'throw(err);' . "\n"
     . Perlito5::Javascript2::tab($level + 1) .    '}' . "\n"
     . Perlito5::Javascript2::tab($level + 1) .    'else {' . "\n"
     . Perlito5::Javascript2::tab($level + 2)
         . ( $has_local
           ? 'return p5cleanup_local(local_idx, err)'
           : 'return(err)'
           )
         . ";\n"
     . Perlito5::Javascript2::tab($level + 1) .   '}' . "\n"
     . Perlito5::Javascript2::tab($level)     . '}';
 
