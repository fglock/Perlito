
package Perlito5;

$VERSION = '9.000';

1;

=head1 Perlito5


    use Perlito5::Compiler;
    use Perlito5::Javascript2::Emitter;
    use Perlito5::Javascript2::Runtime;

    my $perl5_source = ' print "hello, World!\n" ';
    $Perlito5::PKG_NAME = 'main';
    $Perlito5::PROTO    = {};
    my $ast = Perlito5::Grammar::exp_stmts($perl5_source, 0);
    my $js_source = Perlito5::AST::CompUnit::emit_javascript2_program(
        [
            Perlito5::AST::CompUnit->new( name => 'main', body => Perlito5::Match::flat($ast) )
        ]
    );
    print $js_source;


=cut

