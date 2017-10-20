package Sys::Hostname;

use strict;

use Carp;

require Exporter;

our @ISA     = qw/ Exporter /;
our @EXPORT  = qw/ hostname /;

our $VERSION;

our $host;

sub hostname {
    eval {
    return Java::inline("
        new PlString(java.net.InetAddress.getLocalHost().getHostName())
    ");
    }; # UnknownHostException
}

1;

__END__

=head1 NAME

Sys::Hostname - Try every conceivable way to get hostname

=head1 SYNOPSIS

    use Sys::Hostname;
    $host = hostname;

=head1 DESCRIPTION

Attempts several methods of getting the system hostname and
then caches the result.  It tries the first available of the C
library's gethostname(), C<`$Config{aphostname}`>, uname(2),
C<syscall(SYS_gethostname)>, C<`hostname`>, C<`uname -n`>,
and the file F</com/host>.  If all that fails it C<croak>s.

All NULs, returns, and newlines are removed from the result.

=head1 AUTHOR

David Sundstrom E<lt>F<sunds@asictest.sc.ti.com>E<gt>

Texas Instruments

XS code added by Greg Bacon E<lt>F<gbacon@cs.uah.edu>E<gt>

=cut

