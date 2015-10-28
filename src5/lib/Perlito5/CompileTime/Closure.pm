
package Perlito5::CompileTime::Closure;

sub new {
    my ($class, %args) = @_;
    # coderef
    # scope
    # source_code
    bless \%args, $class;
}

sub apply {

}

sub dump {
    # do { my $x = 123; sub { $x + $_[0] } }
}

