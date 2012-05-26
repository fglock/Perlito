use v6;

multi eval($str, :$lang! where 'perl5') {
    my $inp_file = "tmp.p5";
    my $out_file = "tmp.p6";
    my $fh = open($inp_file, :w);
    $fh.print($str);
    $fh.close;
    shell "perl perlito5.pl -Isrc5/lib -Cperl6 $inp_file > $out_file";
    my $p6_str = slurp $out_file;
    # say "[[$p6_str]]";
    $p6_str.eval;
}

my $p5_str = '
    my @x;
    $x[1] = 2+2;
    say "got $x[1]";
';
eval($p5_str, :lang<perl5>);


=begin comments

If I define multi eval($str, :$lang! where 'perl5')
then rakudo can't find eval :lang<perl6> anymore
because eval is declared as an only sub

This is a workaround (by moritz++):

    proto eval(|$) {*};
    multi sub eval($str) {
        $str.eval;
    }

=end comments

