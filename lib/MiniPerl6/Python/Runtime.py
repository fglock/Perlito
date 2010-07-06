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

__all__ = ['mp6_print', 'mp6_say', 'mp6_warn', 'mp6_Undef']

def mp6_print(*msg):
    for m in msg:
        sys.stdout.write(str(m))

def mp6_say(*msg):
    for m in msg:
        sys.stdout.write(str(m))
    sys.stdout.write("\n")

def mp6_warn(*msg):
    for m in msg:
        sys.stderr.write(str(m))
    sys.stderr.write("\n")

class mp6_Undef:
    def __str__(self):
        return ""
    def __int__(self):
        return 0
    def __float__(self):
        return 0.0
    def __nonzero__(self):
        return False

