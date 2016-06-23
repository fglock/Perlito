package List::Util;
use strict;

use Exporter qw(import);
our @EXPORT_OK = qw(
    reduce
);

sub reduce (&@) {
    # PerlOp.reduce(PlArray(PlClosure c, PlArray a));
    Java::inline("PerlOp.reduce(List__)");
}

1;

