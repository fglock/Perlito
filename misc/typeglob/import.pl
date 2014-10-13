package X;
use strict;
no strict "refs";

# use import;
BEGIN { require "import.pm"; import::import( "import" ); }
# BEGIN { require "import.pm"; import::export( "import" ); }
# BEGIN { my $x; *X::v = \$x; }

$v

