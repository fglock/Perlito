
use strict;
print "1..6\n";

my $context;
my $expression_context;
my $t;
my @t;

$context = '';
$expression_context = '';
$t = test_scalar();
print "not " if $context ne 'scalar';
print "ok 1  # $context\n";

print "not " if $expression_context ne 'list';
print "ok 2  # $context\n";

$context = '';
@t = test_list();
print "not " if $context ne 'list';
print "ok 3  # $context\n";


$context = '';
$expression_context = '';
@t = test_scalar();
print "not " if $context ne 'list';
print "ok 4  # $context\n";

print "not " if $expression_context ne 'list';
print "ok 5  # $context\n";

$context = '';
$t = test_list();
print "not " if $context ne 'scalar';
print "ok 6  # $context\n";




sub test_scalar {
    my @x = ( 1, 2, expression_context(), return( context() ), 3 );
}
sub test_list {
    my $x = return( context() );
}

sub context {
    $context = wantarray ? "list" : defined(wantarray) ? "scalar" : "void";
}
sub expression_context {
    $expression_context = wantarray ? "list" : defined(wantarray) ? "scalar" : "void";
}

