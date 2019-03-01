package Perlito5::Perl5::Runtime;

sub eval_ast {
    my ($ast) = @_;
    my $want = 0;

    my @data = $ast->emit_perl5(0, $want);
    my $out = [];
    Perlito5::Perl5::PrettyPrinter::pretty_print( \@data, 0, $out );
    my $code = "package $Perlito5::PKG_NAME; " . join( '', @$out ) . "\n";
    # say STDERR "source: [" . $code . "]";
    Perlito5::set_global_phase("UNITCHECK");
    $_->() while $_ = shift @Perlito5::UNITCHECK_BLOCK;
    $code = "#line $Perlito5::LINE_NUMBER \"$Perlito5::FILE_NAME\"\n" . $code;
    return eval($code);
}

sub emit_perl5 {
    return <<'EOT';
EOT
}

1;

=pod

=head1 NAME

Perlito5::Perl5::Runtime

=head1 DESCRIPTION

Provides runtime routines for the Perlito-in-Perl5 compiled code

=head1 AUTHORS

The Pugs Team.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011, 2012 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
