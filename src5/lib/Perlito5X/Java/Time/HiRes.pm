package Time::HiRes;
use strict;

use Exporter qw(import);
our @EXPORT = qw();
our @EXPORT_OK = qw(time);

package Java::System { import => "java.lang.System" }

sub time {
    return System->currentTimeMillis() * 0.001;
}

1;

__END__

# Test:
# 
# perl perlito5.pl -Isrc5/lib -Isrc5 -Cjava -e ' use Time::HiRes "time"; print time(), "\n"; '  > Main.java ; javac Main.java ; java Main
#

