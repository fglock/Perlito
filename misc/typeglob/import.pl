#
# importing variables like "our"/"use vars" - using typeglobs:
#
# The variables must be created in a *different* package
#
# The variables must be created at BEGIN time

package X;
# sub imp { my $x; *X::v = \$x; }

package Y;
use strict;

# BEGIN { X::imp() }

# BEGIN { my $x; *X::v = \$x; }

# my $x; *X::v = \$x;
# sub imp { my $x; *X::v = \$x; }

package X;
use strict;

# no strict "refs";
# use import;
# BEGIN { require "import.pm"; import::import( "import" ); }
# BEGIN { require "import.pm"; import::export( "import" ); }
# BEGIN { my $x; *X::v = \$x; }
# sub imp { my $x; *X::v = \$x; }

BEGIN { 
    package Y;
    my $x; *X::v = \$x;
}

# Y::imp();

$v

