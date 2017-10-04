package Cwd;
use strict;
use Exporter;
use vars qw(@ISA @EXPORT @EXPORT_OK $VERSION);

$VERSION = '3.67';

@ISA = qw/ Exporter /;
@EXPORT = qw(cwd getcwd fastcwd fastgetcwd);
@EXPORT_OK = qw(chdir abs_path fast_abs_path realpath fast_realpath);

sub getcwd {
    return Java::inline("
        new PlString(PlV.path.toString())
    ");
}

sub abs_path {
    eval {
        return Java::inline("
            new PlString(PlV.path.resolve(List__.aget(0).toString()).toRealPath().toString())
        ");
    }
}

sub cwd { getcwd }
sub fastcwd { getcwd }
sub fastgetcwd { getcwd }
sub realpath { &abs_path }
sub fast_realpath { &abs_path }
sub fast_abs_path { &abs_path }
sub chdir {
    CORE::chdir(@_);
    $ENV{'PWD'} = getcwd();
    1;
}

1;

__END__

# original documentation for Cwd.pm

=head1 NAME

Cwd - get pathname of current working directory

=head1 SYNOPSIS

    use Cwd;
    my $dir = getcwd;

    use Cwd 'abs_path';
    my $abs_path = abs_path($file);

=head1 DESCRIPTION

This module provides functions for determining the pathname of the
current working directory.  It is recommended that getcwd (or another
*cwd() function) be used in I<all> code to ensure portability.

By default, it exports the functions cwd(), getcwd(), fastcwd(), and
fastgetcwd() (and, on Win32, getdcwd()) into the caller's namespace.  


=head2 getcwd and friends

Each of these functions are called without arguments and return the
absolute path of the current working directory.

=over 4

=item getcwd

    my $cwd = getcwd();

Returns the current working directory.

Exposes the POSIX function getcwd(3) or re-implements it if it's not
available.

=item cwd

    my $cwd = cwd();

The cwd() is the most natural form for the current architecture.  For
most systems it is identical to `pwd` (but without the trailing line
terminator).

=item fastcwd

    my $cwd = fastcwd();

A more dangerous version of getcwd(), but potentially faster.

It might conceivably chdir() you out of a directory that it can't
chdir() you back into.  If fastcwd encounters a problem it will return
undef but will probably leave you in a different directory.  For a
measure of extra security, if everything appears to have worked, the
fastcwd() function will check that it leaves you in the same directory
that it started in.  If it has changed it will C<die> with the message
"Unstable directory path, current directory changed
unexpectedly".  That should never happen.

=item fastgetcwd

  my $cwd = fastgetcwd();

The fastgetcwd() function is provided as a synonym for cwd().

=item getdcwd

    my $cwd = getdcwd();
    my $cwd = getdcwd('C:');

The getdcwd() function is also provided on Win32 to get the current working
directory on the specified drive, since Windows maintains a separate current
working directory for each drive.  If no drive is specified then the current
drive is assumed.

This function simply calls the Microsoft C library _getdcwd() function.

=back


=head2 abs_path and friends

These functions are exported only on request.  They each take a single
argument and return the absolute pathname for it.  If no argument is
given they'll use the current working directory.

=over 4

=item abs_path

  my $abs_path = abs_path($file);

Uses the same algorithm as getcwd().  Symbolic links and relative-path
components ("." and "..") are resolved to return the canonical
pathname, just like realpath(3).

=item realpath

  my $abs_path = realpath($file);

A synonym for abs_path().

=item fast_abs_path

  my $abs_path = fast_abs_path($file);

A more dangerous, but potentially faster version of abs_path.

=back

=head2 $ENV{PWD}

If you ask to override your chdir() built-in function, 

  use Cwd qw(chdir);

then your PWD environment variable will be kept up to date.  Note that
it will only be kept up to date if all packages which use chdir import
it from Cwd.


=head1 NOTES

=over 4

=item *

Since the path separators are different on some operating systems ('/'
on Unix, ':' on MacPerl, etc...) we recommend you use the File::Spec
modules wherever portability is a concern.

=item *

Actually, on Mac OS, the C<getcwd()>, C<fastgetcwd()> and C<fastcwd()>
functions are all aliases for the C<cwd()> function, which, on Mac OS,
calls `pwd`.  Likewise, the C<abs_path()> function is an alias for
C<fast_abs_path()>.

=back

=head1 AUTHOR

Originally by the perl5-porters.

Maintained by Ken Williams <KWILLIAMS@cpan.org>

=head1 COPYRIGHT

Copyright (c) 2004 by the Perl 5 Porters.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

Portions of the C code in this library are copyright (c) 1994 by the
Regents of the University of California.  All rights reserved.  The
license on this code is compatible with the licensing of the rest of
the distribution - please see the source code in F<Cwd.xs> for the
details.

=head1 SEE ALSO

L<File::chdir>

=cut
