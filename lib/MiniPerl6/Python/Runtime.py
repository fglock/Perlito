"""
miniperl6.python.runtime

DESCRIPTION

Provides runtime routines for the MiniPerl6-in-Python compiled code

AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

COPYRIGHT

Copyright 2010 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

"""

import sys

def mp6_print(*msg):
    for m in msg:
        sys.stdout.write(str(m))

def mp6_say(*msg):
    for m in msg:
        sys.stdout.write(str(m))
    sys.stdout.write("\n")

