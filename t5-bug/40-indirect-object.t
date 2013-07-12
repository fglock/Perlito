use feature 'say';
use strict;

say "1..50";

{
    package Exists; 
    sub exists { 'called Exists::exists' }      # bultin name
}

{
    package Exists2; 
    sub exists2 { 'called Exists2::exists2' }      # not a bultin name
}

package This;
sub this { 'called This::this' }

my $code;
my $expect;
my $package = __PACKAGE__;
my $object  = bless {}, 'This';
my $count   = 1;

sub show {
    my $result = '';
    my $v = eval(' $result = ' . $code . '; 1 ') || 'fail';
    if ( $v eq 'fail' ) {
        my $err = $@;
        chomp $err;
        print "# code:   <<  $code  >>\n";
        print "# result: $result\n" if $result;
        print "# expect: $expect\n";
        if (!$expect || substr($err, 0, length($expect)) ne $expect) {
            print "# $err\n";
            print "not ";
        }
        say "ok $count";
    }
    else {
        print "# code:   <<  $code  >>\n";
        print "# result: $result\n";
        if ($expect) {
            print "not ";
        }
        say "ok $count";
    }
    $count++;
}


#----- package outside this scope

$code = 'exists2 Exists2';
$expect = '';
show();

$code = 'exists2 Exists2 123';
$expect = '';
show();

$code = 'exists2 Exists2::';
$expect = '';
show();

$code = 'exists2 Exists2(123)';
$expect = '';
show();


#----- package does not exist

$code = 'exists2 NotExist';
$expect = 'Can\'t locate object method "exists2" via package "NotExist" (perhaps you forgot to load "NotExist"?)';
show();

$code = 'exists2 NotExist 123';
$expect = 'Can\'t locate object method "exists2" via package "NotExist" (perhaps you forgot to load "NotExist"?)';
show();

$code = 'exists2 NotExist::';
$expect = 'Can\'t locate object method "exists2" via package "NotExist" (perhaps you forgot to load "NotExist"?)';
show();

$code = 'exists2 NotExist(123)';
$expect = 'Can\'t locate object method "exists2" via package "NotExist" (perhaps you forgot to load "NotExist"?)';
show();



#----- package outside this scope, method is a builtin name

$code = 'exists Exists';
$expect = 'exists argument is not a HASH or ARRAY element or a subroutine';
show();

# # Number found where operator expected
# $code = 'exists Exists 123';
# $expect = 'exists argument is not a HASH or ARRAY element or a subroutine';
# show();

$code = 'exists Exists::';
$expect = 'exists argument is not a HASH or ARRAY element or a subroutine';
show();

$code = 'exists Exists(123)';
$expect = 'exists argument is not a subroutine name';
show();


#----- package inside this scope


$code = 'this This';
$expect = '';
show();

$code = 'this This 123';
$expect = '';
show();

$code = 'this This::';
$expect = '';
show();

$code = 'this This(123)';
$expect = '';
show();


#----- package in a variable

$code = 'this $package';
$expect = '';
show();

$code = 'this $package 123';
$expect = 'syntax error';
show();

$code = 'this $package (123)';
$expect = 'syntax error';
show();


#----- object in a variable

$code = 'this $object';
$expect = '';
show();

$code = 'this $object 123';
$expect = 'syntax error';
show();

$code = 'this $object (123)';
$expect = 'syntax error';
show();


#----- object in a block

$code = 'this {$object}';
$expect = '';
show();

$code = 'this {$object} 123';
$expect = 'syntax error';
show();

$code = 'this {$object} (123)';
$expect = 'syntax error';
show();

$code = 'this { a => 123, b => 456 }';
$expect = '';
show();

#----- object in parentheses

$code = 'this ($object)';
$expect = '';
show();

# # Number found where operator expected
# $code = 'this ($object 123)';
# $expect = 'syntax error';
# show();

# # Number found where operator expected
# $code = 'this ($object 123)';
# $expect = 'syntax error';
# show();


#----- unknown sub with scalar

$code = 'not_exist { a => 123, b => 456 }';
$expect = 'Can\'t locate object method "not_exist" via package "a"';
show();

$code = 'not_exist {(a => 123, b => 456)}';
$expect = 'Can\'t locate object method "not_exist" via package "a"';
show();

# # Number found where operator expected
# $code = 'not_exist 123';
# $expect = '';
# show();

$code = 'not_exist $object';
$expect = 'Can\'t locate object method "not_exist" via package "This"';
show();

$code = 'not_exist $object 123';
$expect = 'Can\'t locate object method "not_exist" via package "This"';
show();

$code = 'not_exist $object (123)';
$expect = 'Can\'t locate object method "not_exist" via package "This"';
show();



#----- glob 

$code = 'this STDOUT';
$expect = 'Can\'t locate object method "this" via package "IO::File"';
show();

# Number found where operator expected
$code = 'this STDOUT 123';
$expect = 'Can\'t locate object method "this" via package "IO::File"';
show();

# Number found where operator expected
$code = 'this STDOUT 123';
$expect = 'Can\'t locate object method "this" via package "IO::File"';
show();




#----- glob with parentheses

$code = 'this (STDOUT)';
$expect = 'Bareword "STDOUT" not allowed while "strict subs" in use';
show();

# # Number found where operator expected
# $code = 'this (STDOUT 123)';
# $expect = 'syntax error';
# show();
# 
# # Number found where operator expected
# $code = 'this (STDOUT 123)';
# $expect = 'syntax error';
# show();



#----- print and say

$code = 'print STDOUT';
$expect = '';
show;

$code = 'say STDOUT "# 123"';
$expect = '';
show;

$code = 'say STDOUT "# 123"';
$expect = '';
show;

$code = 'say STDOUT "# 123"';
$expect = '';
show;



#----- special syntax for print and say

$code = 'print (STDOUT)';
$expect = '';
show();

$code = 'say STDOUT "# 123"';
$expect = '';
show();

$code = 'say (STDOUT "# 123")';
$expect = '';
show();

$code = 'say (STDOUT "# 123")';
$expect = '';
show();


#----- print and say with scalar

# # prints the object
# $code = 'print $object';
# $expect = '';
# show;

$code = 'say $object "# 123"';
$expect = 'Not a GLOB reference';
show;

$code = 'say $object "# 123"';
$expect = 'Not a GLOB reference';
show;

$code = 'say $object "# 123"';
$expect = 'Not a GLOB reference';
show;


#----- print and say with structure

# # prints the object
# $code = 'print $object';
# $expect = '';
# show;

# # String found where operator expected
# $code = 'say $object->[0] "# 123"';
# $expect = '';
# show;
 
# # String found where operator expected
# $code = 'say $object->[0] "# 123"';
# $expect = '';
# show;
 
# # String found where operator expected
# $code = 'say $object->[0] "# 123"';
# $expect = '';
# show;


#----- print and say with block

# prints the object
$code = 'print {$object->[0]}';
$expect = 'syntax error';
show;

# String found where operator expected
$code = 'say {$object->[0]} "# 123"';
$expect = 'Not an ARRAY reference';
show;

$code = 'say {$object->[0]} "# 123"';
$expect = 'Not an ARRAY reference';
show;

$code = 'say {$object->[0]} "# 123"';
$expect = 'Not an ARRAY reference';
show;



__END__
 
