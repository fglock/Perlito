package Perlito5::Dumper;

sub Dumper {
    my $obj = $_[0];
    my $level = $_[1] || 0;

    my $ref = ref($obj);
    my $tab = '    ' x $level;
    my $tab1 = $tab . '    ';

    if ($ref eq 'ARRAY') {
        return "[\n" 
            . join( "", 
                    map($tab1 . Dumper($_, $level+1) . ",\n", @$obj)
                  ) 
            . $tab . ']';
    }
    elsif ($ref eq 'HASH') {
        return "{\n"
            . join( "", 
                    map($tab1 . "'$_' => " . Dumper($obj->{$_}, $level+1) . ",\n", sort keys %$obj)
                  )
            . $tab . '}';
    }
    elsif ($ref) {
        # TODO find out what kind of reference this is (ARRAY, HASH, ...)
        # local $@;
        # eval {
        #     my @data = @$obj;
        #     say "is array";
        #     return 'bless(' . "..." . ", '$ref')";
        # }
        # or eval {
        #     $@ = '';
        #     my %data = %$obj;
        #     say "is hash";
        #     return 'bless(' . "..." . ", '$ref')";
        # };
        # $@ = '';

        return 'bless(' . "..." . ", '$ref')";
    }

    return "'$obj'";

}

1;

