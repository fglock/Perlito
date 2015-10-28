
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

# TODO - problem with identifying shared variables in js,
# because it doesn't use "real" scalar references
#
# $ node perlito5.js -I src5/lib -e ' my $x = 123; my $v = \$x; my @x = ( $v, \$x ); print $_,"\n" for @x '
# SCALAR(0x5b143b76)
# SCALAR(0x5b143b77)    -- different ref value
#
# $ perl -e ' use Data::Dumper; my $x = 123; my $v = \$x; my @x = ( $v, \$x ); print $_,"\n" for @x '
# SCALAR(0x7fc01902af60)
# SCALAR(0x7fc01902af60)
#
