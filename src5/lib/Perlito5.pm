package Perlito5;

$VERSION = '9.012';

1;

=pod

=head1 NAME

Perlito5 - a Perl5 compiler

=head1 SYNOPSIS

    use Perlito5::Compiler;
    use Perlito5::Javascript2::Emitter;
    use Perlito5::Javascript2::Runtime;

    my $perl5_source = ' print "hello, World!\n" ';
    $Perlito5::PKG_NAME = 'main';
    $Perlito5::PROTO    = {};
    my $ast = Perlito5::Grammar::exp_stmts($perl5_source, 0);
    print Dumper $ast;
    my $js_source = Perlito5::AST::CompUnit::emit_javascript2_program([
        Perlito5::AST::CompUnit->new( name => 'main', body => Perlito5::Match::flat($ast) )
    ]);
    print $js_source;


    # in the command line:

    perlito5 --help

    perlito5 -Cjs program.pl

=head1 DESCRIPTION

This program reads Perl5 source code and generates native code.

The compiler options are available with the command:

    perlito5 -h

=head1 COOKBOOK

=head2 Obtaining a "perlito5.js" script that executes in the "node.js" Javascript compiler

    # Step 1: use perlito5 to compile perlito5 to Javascript.
    perlito5 --bootstrapping -Cjs `which perlito5` > perlito5.js

    # Step 2: ensure that PERL5LIB points to the directory where Perlito5.pm is installed.
    echo $PERL5LIB

    # Step 3: set PERL5LIB if needed.
    perldoc -l Perlito5
       # /usr/local/lib/perl5/site_perl/5.20.1/Perlito5.pm
    export PERL5LIB=/usr/local/lib/perl5/site_perl/5.20.1

    # Step 4: install nodejs if needed.

    # Step 5: this should work now:
    nodejs perlito5.js -e ' print "hello, World!\n" '
        # hello, World!


=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.

=head1 SEE ALSO

L<http://fglock.github.io/Perlito>

=head1 COPYRIGHT

Copyright 2011, 2012, 2015 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://dev.perl.org/licenses/artistic.html>

=cut

