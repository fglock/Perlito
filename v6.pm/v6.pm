package v6;
$v6::VERSION = '0.033';
use strict;

sub import {
    my ($module,@args) = @_;
    my $submodule = $args[0];
    if ($submodule =~ /^-/) {
        $submodule =~ s/^-//;
        $submodule = 'v6::'.$submodule;
        eval("require $submodule");
        die if $@;
        $submodule->import(@args);
    }
}

1;

=head1 NAME 

v6 - A Perl 6 implementation

=head1 SYNOPSIS

    # file: hello_world.pl
    use v6-perlito;
    "hello, World".say;

    $ perl hello_world.pl

=head1 DESCRIPTION

The C<v6> module is a front-end to the "Perlito" compiler.

Alternate backend modules can be installed. For example, the "Mildew" compiler can be used as:

  use v6::mildew;

=head1 REQUIREMENTS

- The source file header must be valid perl5 I<and> perl6 code.

This is a valid header:

    #!/usr/bin/perl
    use v6-perlito;

* it executes perl5

* perl5 will call the C<v6.pm> module.

This is an invalid header:

    #!/usr/bin/pugs
    use v6;

* it tells perl5 to execute C</usr/bin/pugs>.

* it would tell perl5 that Perl v6.0.0 required.

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://perl6.org>.

The Perlito compiler at L<http://github.com/fglock/Perlito>.

=head1 COPYRIGHT

Copyright 2006, 2010 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
