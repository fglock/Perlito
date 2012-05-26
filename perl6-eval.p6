use v6;

multi eval($str, :$lang! where 'perl5') {
    my $inp_file = "tmp.p5";
    my $out_file = "tmp.p6";
    my $fh = open($inp_file, :w);
    $fh.print($str);
    $fh.close;
    shell "perl perlito5.pl -Isrc5/lib -Cperl6 $inp_file > $out_file";
    my $p6_str = slurp $out_file;
    say "[[$p6_str]]";
    eval($p6_str, :lang<perl6>);
}

my $p5_str = "say 'hello, World!'";
#eval $p5_str;
eval($p5_str, :lang<perl5>);

