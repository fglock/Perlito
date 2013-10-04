use strict;

package Perlito5::PrettyPrint {

    my %dispatch = (
        stmt    => \&statement,
        block   => \&block,
        list    => \&list,
        keyword => \&keyword,
    );

    # TODO
    # precedence
    # prefix/infix/postfix

    sub tab {
        my $level = shift;
        "\t" x $level;
    }

    sub keyword {
        my ( $data, $level, $out ) = @_;
        push @$out, $data;
        return;
    }

    sub statement {
        my ( $data, $level, $out ) = @_;
        if ( !ref($data) ) {
            push @$out, $data;
            return;
        }
        for my $line ( 0 .. $#$data ) {
            my $d = $data->[$line];
            if ( ref($d) ) {
                my @dd = @$d;
                my $cmd = shift @dd;
                $dispatch{ $cmd }->( \@dd, $level, $out );
            }
            else {
                push @$out, $d;
            }
        }
    }

    sub block {
        my ( $data, $level, $out ) = @_;
        if ( @$data == 0 ) {
            push @$out, "{}";
            return;
        }
        push @$out, '{', "\n";
        $level++;
        for my $line ( 0 .. $#$data ) {
            my $d    = $data->[$line];
            my $out1 = [];
            if ( ref($d) ) {
                my @dd = @$d;
                my $cmd = shift @dd;
                $dispatch{ $cmd }->( \@dd, $level, $out1 );
            }
            else {
                push @$out1, $d;
            }
            push @$out, tab($level), @$out1;
            push @$out, ';' if $line != $#$data;
            push @$out, "\n";
        }
        $level--;
        push @$out, tab($level), '}';
    }

    sub list {
        my ( $data, $level, $out ) = @_;
        if ( @$data == 0 ) {
            return;
        }
        my $op = shift @$data;
        push @$out, '(';
        $level++;
        for my $line ( 0 .. $#$data ) {
            my $d    = $data->[$line];
            my $out1 = [];
            if ( ref($d) ) {
                $dispatch{ $d->[0] }->( $d->[1], $level, $out1 );
            }
            else {
                push @$out1, $d;
            }
            push @$out, @$out1;
            push @$out, $op if $line != $#$data;
        }
        $level--;
        push @$out, ')';
    }

}

{
    my $data = [
        [ 'stmt', [ keyword => 'if' ] ],
        [ 'block', [ 'stmt' => '2' ], [ 'stmt' => '3' ], ],
        ['block'],
        [ 'list', ', ', '2', '3' ],
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
 
