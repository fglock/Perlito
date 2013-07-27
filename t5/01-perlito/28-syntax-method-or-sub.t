use feature 'say';

print "1..3\n";

my $v = 1;

sub Foo::method {
    print "ok 2 - Foo method\n";
}

sub Bar::method {
    print "ok $v - Bar method\n";
    $v += 2;
}

sub Foo {
    return bless [] => "Bar";
}

Foo->method;
Foo::->method;
Foo()->method;

