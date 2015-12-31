use strict;

package J::User { import => "org.perlito.examples.JUser" };

package Boolean { };

package String { };

################################
# Declare (extend & override):
package P::User {
    extends => 'J::User',
    'Java::inline' => '
        public PUser(String name, String email) {
            super(name,email);
        }
    ',
    methods => [
        validateName => {
            decl   => ["public"],
            args   => ['String'],
            return => 'Boolean',
            code   => "impl::validateName"
        },
        validateEmail => {
            decl   => ["public"],
            args   => ['String'],
            return => 'Boolean',
            code   => "impl::validateEmail"
        },
    ],
};

################################
# Implement overrided methods:
package impl;

sub validateName {
    my $self = shift;
    my $name = shift;
    my $result;
    if ($name=~m/^[a-zA-Z]*$/g) {
        $result = Boolean->TRUE;
        print "perl: $name is a valid name\n";
    }
    else {
        $result = Boolean->FALSE;
        print "perl: $name is not a valid name\n";
    }
    return $result;
}

sub validateEmail {
    my $self  = shift;
    my $email = shift;
    my $pattern = get_email_pattern();
    my $result;
    
    my @match = $email=~/$pattern/g;

    if (@match and ($match[0] eq $email)) {
        $result = Boolean->TRUE;
        print "perl: $email is a valid email\n";
    }
    else {
        $result = Boolean->FALSE;
        print "perl: $email is not a valid email\n";
    }
    return $result;
}

sub get_email_pattern {

    return '(^(?:[a-z0-9][-a-z0-9_\+\.]*[a-z0-9])@(?:[a-z0-9][-a-z0-9\.]*[a-z0-9])\.(?:(?:arpa|root|aero|biz|cat|com|coop|edu|gov|info|int|jobs|mil|mobi|museum|name|net|org|pro|tel|travel|ac|ad|ae|af|ag|ai|al|am|an|ao|aq|ar|as|at|au|aw|ax|az|ba|bb|bd|be|bf|bg|bh|bi|bj|bm|bn|bo|br|bs|bt|bv|bw|by|bz|ca|cc|cd|cf|cg|ch|ci|ck|cl|cm|cn|co|cr|cu|cv|cx|cy|cz|de|dj|dk|dm|do|dz|ec|ee|eg|er|es|et|eu|fi|fj|fk|fm|fo|fr|ga|gb|gd|ge|gf|gg|gh|gi|gl|gm|gn|gp|gq|gr|gs|gt|gu|gw|gy|hk|hm|hn|hr|ht|hu|id|ie|il|im|in|io|iq|ir|is|it|je|jm|jo|jp|ke|kg|kh|ki|km|kn|kr|kw|ky|kz|la|lb|lc|li|lk|lr|ls|lt|lu|lv|ly|ma|mc|md|mg|mh|mk|ml|mm|mn|mo|mp|mq|mr|ms|mt|mu|mv|mw|mx|my|mz|na|nc|ne|nf|ng|ni|nl|no|np|nr|nu|nz|om|pa|pe|pf|pg|ph|pk|pl|pm|pn|pr|ps|pt|pw|py|qa|re|ro|ru|rw|sa|sb|sc|sd|se|sg|sh|si|sj|sk|sl|sm|sn|so|sr|st|su|sv|sy|sz|tc|td|tf|tg|th|tj|tk|tl|tm|tn|to|tp|tr|tt|tv|tw|tz|ua|ug|uk|um|us|uy|uz|va|vc|ve|vg|vi|vn|vu|wf|ws|ye|yt|yu|za|zm|zw)|(?:[0-9]{1,3}\.{3}[0-9]{1,3}))$)';

}


################################
# Main:
package main;

my $mail = shift @ARGV;

my P::User $user = P::User->new('John', $mail->toString() );

my $n = $user->getName();
my $e = $user->getEmail();

print "created used $n with email $e\n";

