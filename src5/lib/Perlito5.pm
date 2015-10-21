package Perlito5;

$VERSION = '9.003';

1;

=head1 Perlito5

=head1 SYNOPSIS

    use Perlito5::Compiler;
    use Perlito5::Javascript2::Emitter;
    use Perlito5::Javascript2::Runtime;

    my $perl5_source = ' print "hello, World!\n" ';
    $Perlito5::PKG_NAME = 'main';
    $Perlito5::PROTO    = {};
    my $ast = Perlito5::Grammar::exp_stmts($perl5_source, 0);
    print Dumper $ast;
    my $js_source = Perlito5::AST::CompUnit::emit_javascript2_program(
        [
            Perlito5::AST::CompUnit->new( name => 'main', body => Perlito5::Match::flat($ast) )
        ]
    );
    print $js_source;


=head1 COMMAND LINE

    perlito5 --help

    perlito5 -Cjs program.pl

=head1 DESCRIPTION

This program reads Perl5 source code and generates native code.

The compiler options are available with the command:

    perlito5 -h

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

