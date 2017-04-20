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
    # warn "in eval BASE_SCOPE exit: ", Data::Dumper::Dumper($Perlito5::BASE_SCOPE);
    $code = "#line $Perlito5::LINE_NUMBER\n" . $code;
    return eval($code);
}

sub emit_perl5 {
    return <<'EOT';

use v5.10;
use feature 'say';

EOT
}

1;

=pod

=head1 NAME

Perlito5::Perl5::Runtime

=head1 DESCRIPTION

Provides runtime routines for the Perlito-in-Perl5 compiled code

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011, 2012 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
