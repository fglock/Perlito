package Fcntl;

=head1 NAME

Fcntl - load the C Fcntl.h defines

=head1 SYNOPSIS

    use Fcntl;
    use Fcntl qw(:DEFAULT :flock);

=head1 DESCRIPTION

This module is just a translation of the C F<fcntl.h> file.
Unlike the old mechanism of requiring a translated F<fcntl.ph>
file, this uses the B<h2xs> program (see the Perl source distribution)
and your native C compiler.  This means that it has a 
far more likely chance of getting the numbers right.

=head1 NOTE

Only C<#define> symbols get translated; you must still correctly
pack up your own arguments to pass as args for locking functions, etc.

=head1 EXPORTED SYMBOLS

By default your system's F_* and O_* constants (eg, F_DUPFD and
O_CREAT) and the FD_CLOEXEC constant are exported into your namespace.

You can request that the flock() constants (LOCK_SH, LOCK_EX, LOCK_NB
and LOCK_UN) be provided by using the tag C<:flock>.  See L<Exporter>.

You can request that the old constants (FAPPEND, FASYNC, FCREAT,
FDEFER, FEXCL, FNDELAY, FNONBLOCK, FSYNC, FTRUNC) be provided for
compatibility reasons by using the tag C<:Fcompat>.  For new
applications the newer versions of these constants are suggested
(O_APPEND, O_ASYNC, O_CREAT, O_DEFER, O_EXCL, O_NDELAY, O_NONBLOCK,
O_SYNC, O_TRUNC).

For ease of use also the SEEK_* constants (for seek() and sysseek(),
e.g. SEEK_END) and the S_I* constants (for chmod() and stat()) are
available for import.  They can be imported either separately or using
the tags C<:seek> and C<:mode>.

Please refer to your native fcntl(2), open(2), fseek(3), lseek(2)
(equal to Perl's seek() and sysseek(), respectively), and chmod(2)
documentation to see what constants are implemented in your system.

See L<perlopentut> to learn about the uses of the O_* constants
with sysopen().

See L<perlfunc/seek> and L<perlfunc/sysseek> about the SEEK_* constants.

See L<perlfunc/stat> about the S_I* constants.

=cut

use strict;
our($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);

require Exporter;
#require XSLoader;
@ISA = qw(Exporter);
$VERSION = '1.13';

#XSLoader::load();

# Named groups of exports
%EXPORT_TAGS = (
    'flock'   => [qw(LOCK_SH LOCK_EX LOCK_NB LOCK_UN)],
    'Fcompat' => [qw(FAPPEND FASYNC FCREAT FDEFER FDSYNC FEXCL FLARGEFILE
		     FNDELAY FNONBLOCK FRSYNC FSYNC FTRUNC)],
    'seek'    => [qw(SEEK_SET SEEK_CUR SEEK_END)],
    'mode'    => [qw(S_ISUID S_ISGID S_ISVTX S_ISTXT
		     _S_IFMT S_IFREG S_IFDIR S_IFLNK
		     S_IFSOCK S_IFBLK S_IFCHR S_IFIFO S_IFWHT S_ENFMT
		     S_IRUSR S_IWUSR S_IXUSR S_IRWXU
		     S_IRGRP S_IWGRP S_IXGRP S_IRWXG
		     S_IROTH S_IWOTH S_IXOTH S_IRWXO
		     S_IREAD S_IWRITE S_IEXEC
		     S_ISREG S_ISDIR S_ISLNK S_ISSOCK
		     S_ISBLK S_ISCHR S_ISFIFO
		     S_ISWHT S_ISENFMT		
		     S_IFMT S_IMODE
                  )],
);

# Items to export into callers namespace by default
# (move infrequently used names to @EXPORT_OK below)
@EXPORT =
  qw(
	FD_CLOEXEC
	F_ALLOCSP
	F_ALLOCSP64
	F_COMPAT
	F_DUP2FD
	F_DUPFD
	F_EXLCK
	F_FREESP
	F_FREESP64
	F_FSYNC
	F_FSYNC64
	F_GETFD
	F_GETFL
	F_GETLK
	F_GETLK64
	F_GETOWN
	F_NODNY
	F_POSIX
	F_RDACC
	F_RDDNY
	F_RDLCK
	F_RWACC
	F_RWDNY
	F_SETFD
	F_SETFL
	F_SETLK
	F_SETLK64
	F_SETLKW
	F_SETLKW64
	F_SETOWN
	F_SHARE
	F_SHLCK
	F_UNLCK
	F_UNSHARE
	F_WRACC
	F_WRDNY
	F_WRLCK
	O_ACCMODE
	O_ALIAS
	O_APPEND
	O_ASYNC
	O_BINARY
	O_CREAT
	O_DEFER
	O_DIRECT
	O_DIRECTORY
	O_DSYNC
	O_EXCL
	O_EXLOCK
	O_LARGEFILE
	O_NDELAY
	O_NOCTTY
	O_NOFOLLOW
	O_NOINHERIT
	O_NONBLOCK
	O_RANDOM
	O_RAW
	O_RDONLY
	O_RDWR
	O_RSRC
	O_RSYNC
	O_SEQUENTIAL
	O_SHLOCK
	O_SYNC
	O_TEMPORARY
	O_TEXT
	O_TRUNC
	O_WRONLY
     );

# Other items we are prepared to export if requested
@EXPORT_OK = (qw(
	DN_ACCESS
	DN_ATTRIB
	DN_CREATE
	DN_DELETE
	DN_MODIFY
	DN_MULTISHOT
	DN_RENAME
	F_GETLEASE
	F_GETPIPE_SZ
	F_GETSIG
	F_NOTIFY
	F_SETLEASE
	F_SETPIPE_SZ
	F_SETSIG
	LOCK_MAND
	LOCK_READ
	LOCK_RW
	LOCK_WRITE
        O_ALT_IO
        O_EVTONLY
	O_IGNORE_CTTY
	O_NOATIME
	O_NOLINK
        O_NOSIGPIPE
	O_NOTRANS
        O_SYMLINK
        O_TTY_INIT
), map {@{$_}} values %EXPORT_TAGS);


# $ perl -e ' use Fcntl; use Data::Dumper; for my $k ( sort keys %Fcntl:: ) { eval { $v = &{"Fcntl::$k"}; print "sub $k () { ", (length($v) ? $v : defined $v ? "q{$v}" : "undef" ) , " }\n"; } } '
sub FAPPEND () { 8 }
sub FASYNC () { 64 }
sub FD_CLOEXEC () { 1 }
sub FNDELAY () { 4 }
sub FNONBLOCK () { 4 }
sub F_DUPFD () { 0 }
sub F_GETFD () { 1 }
sub F_GETFL () { 3 }
sub F_GETLK () { 7 }
sub F_GETOWN () { 5 }
sub F_RDLCK () { 1 }
sub F_SETFD () { 2 }
sub F_SETFL () { 4 }
sub F_SETLK () { 8 }
sub F_SETLKW () { 9 }
sub F_SETOWN () { 6 }
sub F_UNLCK () { 2 }
sub F_WRLCK () { 3 }
sub LOCK_EX () { 2 }
sub LOCK_NB () { 4 }
sub LOCK_SH () { 1 }
sub LOCK_UN () { 8 }
sub O_ACCMODE () { 3 }
sub O_APPEND () { 8 }
sub O_ASYNC () { 64 }
sub O_BINARY () { 0 }
sub O_CREAT () { 512 }
sub O_DIRECTORY () { 1048576 }
sub O_DSYNC () { 4194304 }
sub O_EXCL () { 2048 }
sub O_EXLOCK () { 32 }
sub O_NDELAY () { 4 }
sub O_NOCTTY () { 131072 }
sub O_NOFOLLOW () { 256 }
sub O_NONBLOCK () { 4 }
sub O_RDONLY () { 0 }
sub O_RDWR () { 2 }
sub O_SHLOCK () { 16 }
sub O_SYNC () { 128 }
sub O_TEXT () { 0 }
sub O_TRUNC () { 1024 }
sub O_WRONLY () { 1 }
sub SEEK_CUR () { 1 }
sub SEEK_END () { 2 }
sub SEEK_SET () { 0 }
sub S_IEXEC () { 64 }
sub S_IFBLK () { 24576 }
sub S_IFCHR () { 8192 }
sub S_IFDIR () { 16384 }
sub S_IFIFO () { 4096 }
sub S_IFLNK () { 40960 }
sub S_IFMT () { 61440 }
sub S_IFREG () { 32768 }
sub S_IFSOCK () { 49152 }
sub S_IFWHT () { 57344 }
sub S_IMODE () { 0 }
sub S_IREAD () { 256 }
sub S_IRGRP () { 32 }
sub S_IROTH () { 4 }
sub S_IRUSR () { 256 }
sub S_IRWXG () { 56 }
sub S_IRWXO () { 7 }
sub S_IRWXU () { 448 }
sub S_ISBLK () { q{} }
sub S_ISCHR () { q{} }
sub S_ISDIR () { q{} }
sub S_ISFIFO () { q{} }
sub S_ISGID () { 1024 }
sub S_ISLNK () { q{} }
sub S_ISREG () { q{} }
sub S_ISSOCK () { q{} }
sub S_ISTXT () { 512 }
sub S_ISUID () { 2048 }
sub S_ISVTX () { 512 }
sub S_ISWHT () { q{} }
sub S_IWGRP () { 16 }
sub S_IWOTH () { 2 }
sub S_IWRITE () { 128 }
sub S_IWUSR () { 128 }
sub S_IXGRP () { 8 }
sub S_IXOTH () { 1 }
sub S_IXUSR () { 64 }
sub _S_IFMT () { 61440 }
sub bootstrap () { 1 }

1;
