package Perlito5;
use Perlito5::Grammar::Scope;
use strict;

our @PERL_VERSION = ( 5, 26, 0 );

$^O = 'perlito5'    unless defined $^O;
$/  = chr(10)       unless defined $/;
$"  = ' '           unless defined $";
$,  = undef         unless defined $,;
$!  = ''            unless defined $!;
$;  = chr(28)       unless defined $;;
$?  = 0             unless defined $?;
# $[  = 0             unless defined $[;    # "assignment to $[ is deprecated"
$]  = sprintf( "%d.%03d%03d", @PERL_VERSION )    unless $];      # $] is defined(), but ${"main::]"} is not
$^V = bless( { 'original' => 'v' . join( ".", @PERL_VERSION ),   # v5.26.0
               'qv'       => 1,
               'version'  => [ @PERL_VERSION ]
             }, 'version' )
                    unless defined $^V;
$^H = 0;
%^H = ();

our $EXPAND_USE   = 1;
our $EMIT_USE     = 0;
our $WARNINGS     = 0;
our $UTF8         = 0;
our $BYTES        = 0;
our @CALLER       = ();
our %DATA_SECTION = ();   # contents of the __DATA__ sections per package
our $PKG_NAME     = '';   # current package being compiled
our $LINE_NUMBER  = 0;    # current line number being compiled
our $FILE_NAME    = '';   # current file name being compiled
our %FORMAT       = ();   # 'format' statements - hash of subs

# information about the current compilation process
our $GLOBAL          = {};
our @BASE_SCOPE      = ( Perlito5::Grammar::Scope->new_base_scope() );
our $CLOSURE_SCOPE   = 0;    # variables that are in scope in the current closure being compiled
our @SCOPE_STMT      = ();
our @END_BLOCK       = ();    # END block LIFO - array of subs
our @INIT_BLOCK      = ();    # INIT block FIFO - array of subs
our @CHECK_BLOCK     = ();    # CHECK block LIFO - array of subs
our @UNITCHECK_BLOCK = ();    # UNITCHECK block LIFO - array of subs
our %BEGIN_SCRATCHPAD = ();   # list of "my" variables captured in BEGIN blocks
our $PROTO           = {};
our %VARS            = ();    # implements "use vars"

sub init_proto {
    $PROTO = {};
    # These pseudo-subroutines are "predeclared"
    $PROTO->{"Java::inline"} = undef;
    $PROTO->{"JS::inline"} = undef;
}

# $Perlito5::STRICT_* - See Perlito5X::strict.pm
# from perl.h in Perl source
$Perlito5::HINT_INTEGER        = 0x00000001;    #  integer pragma
$Perlito5::HINT_STRICT_REFS    = 0x00000002;    #  strict pragma
$Perlito5::HINT_LOCALE         = 0x00000004;    #  locale pragma
$Perlito5::HINT_BYTES          = 0x00000008;    #  bytes pragma
$Perlito5::HINT_LOCALE_PARTIAL = 0x00000010;    #  locale, but a subset of categories

$Perlito5::HINT_EXPLICIT_STRICT_REFS = 0x00000020;    #  strict.pm
$Perlito5::HINT_EXPLICIT_STRICT_SUBS = 0x00000040;    #  strict.pm
$Perlito5::HINT_EXPLICIT_STRICT_VARS = 0x00000080;    #  strict.pm

$Perlito5::HINT_BLOCK_SCOPE = 0x00000100;
$Perlito5::HINT_STRICT_SUBS = 0x00000200;             #  strict pragma
$Perlito5::HINT_STRICT_VARS = 0x00000400;             #  strict pragma
$Perlito5::HINT_UNI_8_BIT   = 0x00000800;             #  unicode_strings feature

#  The HINT_NEW_* constants are used by the overload pragma
$Perlito5::HINT_NEW_INTEGER    = 0x00001000;
$Perlito5::HINT_NEW_FLOAT      = 0x00002000;
$Perlito5::HINT_NEW_BINARY     = 0x00004000;
$Perlito5::HINT_NEW_STRING     = 0x00008000;
$Perlito5::HINT_NEW_RE         = 0x00010000;
$Perlito5::HINT_LOCALIZE_HH    = 0x00020000;          #  %^H needs to be copied
$Perlito5::HINT_LEXICAL_IO_IN  = 0x00040000;          #  ${^OPEN} is set for input
$Perlito5::HINT_LEXICAL_IO_OUT = 0x00080000;          #  ${^OPEN} is set for output

$Perlito5::HINT_RE_TAINT = 0x00100000;                #  re pragma
$Perlito5::HINT_RE_EVAL  = 0x00200000;                #  re pragma

$Perlito5::HINT_FILETEST_ACCESS = 0x00400000;         #  filetest pragma
$Perlito5::HINT_UTF8            = 0x00800000;         #  utf8 pragma

$Perlito5::HINT_NO_AMAGIC = 0x01000000;               #  overloading pragma

$Perlito5::HINT_RE_FLAGS = 0x02000000;                #  re '/xism' pragma

$Perlito5::HINT_FEATURE_MASK = 0x1c000000;            #  3 bits for feature bundles

#  The following are stored in $^H{sort}, not in PL_hints
$Perlito5::HINT_SORT_STABLE   = 0x00000100;           #  sort styles
$Perlito5::HINT_SORT_UNSTABLE = 0x00000200;

my %constants = (
              'integer'   => $Perlito5::HINT_NEW_INTEGER,
              'float'     => $Perlito5::HINT_NEW_FLOAT,
              'binary'    => $Perlito5::HINT_NEW_BINARY,
              'q'         => $Perlito5::HINT_NEW_STRING,
              'qr'        => $Perlito5::HINT_NEW_RE,
             );


# the Perl-to-Java compiler uses this syntax for "annotations":
#   package Put { import => 'java.Put' };
# annotations are stored as namespace + AST
our @ANNOTATION;

sub set_global_phase {
    my $phase = shift;
    local $@;
    eval { ${^GLOBAL_PHASE} = $phase };
}

our $ID           = 100;    # generic "id" source; increment after use

# list of packages that "exist" - this is used by the indirect-object parser
our $PACKAGES = {
    STDERR       => 1,
    STDOUT       => 1,
    STDIN        => 1,
    main         => 1,
    strict       => 1,
    warnings     => 1,
    utf8         => 1,
    bytes        => 1,
    encoding     => 1,
    UNIVERSAL    => 1,
    CORE         => 1,
    'CORE::GLOBAL' => 1,
    'Perlito5::IO' => 1,
};

push @INC, $_
    for split ":", ($ENV{PERL5LIB} || "");

# magic numbers - See lib/integer.pm in perl distribution
$Perlito5::INTEGER = 0x01;
# magic numbers - See lib/overloading.pm in perl distribution
$Perlito5::HINT_NO_AMAGIC = 0x01000000;

# these operators change when "use integer" - ( $^H & $Perlito5::INTEGER )
%Perlito5::Integer = (
    '%'   => 1,
    '>>'  => 1,
    '<<'  => 1,
    '^'   => 1,
    '&'   => 1,
    '|'   => 1,
    '+'   => 1,
    '-'   => 1,
    '*'   => 1,
    '/'   => 1,

    '%='  => 1,
    '>>=' => 1,
    '<<=' => 1,
    '^='  => 1,
    '&='  => 1,
    '|='  => 1,
    '+='  => 1,
    '-='  => 1,
    '*='  => 1,
    '/='  => 1,

    '=='  => 1,
    '!='  => 1,
    '>'   => 1,
    '>='  => 1,
    '<'   => 1,
    '<='  => 1,
    '<=>' => 1,
);

# the special variables list
# obtained with:
# $ perldoc -u perlvar | perl -ne ' /^\s*$/ && next; if (/^=item\s+([^\n]+)/) { push @item, $1; print "@item - $_" } else { if (@item) { push @xx, [@item]; print "push\n"; @item = () } }; END {use Data::Dumper; print Dumper \@xx} '

our $SPECIAL_VAR = {
          '$_' => 'ARG',
          '$&' => '$MATCH',
          '$`' => '$PREMATCH',
          '$\'' => '$POSTMATCH',
          '$+' => '$LAST_PAREN_MATCH',
          '@+' => '@LAST_MATCH_END',
          '%+' => '%LAST_PAREN_MATCH',

          '@-' => '@LAST_MATCH_START',
          '$|' => 'autoflush',
          '$/' => '$RS',
          '@_' => '@ARG',
          '< $' => '$EUID',
          '$.' => '$NR',
          '< $< ' => '$UID',
          '$(' => '$GID',
          '$#' => undef,
          '$@' => '$EVAL_ERROR',
          '$=' => '$FORMAT_LINES_PER_PAGE',
          '$,' => '$OFS',
          '$?' => '$CHILD_ERROR',
          '$*' => undef,
          '$[' => undef,
          '$$' => '$PID',
          '%-' => undef,
          '$~' => '$FORMAT_NAME',
          '$-' => '$FORMAT_LINES_LEFT',
          '$&' => '$MATCH',
          '$%' => '$FORMAT_PAGE_NUMBER',
          '$)' => '$EGID',
          '$]' => undef,
          '$!' => '$ERRNO',
          '$;' => '$SUBSEP',
          '$\\' => '$ORS',
          '%!' => undef,
          '$"' => '$LIST_SEPARATOR',
          '$_' => '$ARG',
          '$:' => 'FORMAT_LINE_BREAK_CHARACTERS'
        };

# This list was generated by misc/Perl5/core_redefine.pl
#
# These builtins can be user-redefined using plain subs.
# No other builtins can be redefined (the ones in $Perlito5::CORE_PROTO).
# The other builtins can still be used for method names.

our $CORE_OVERRIDABLE = {
    '__FILE__'         => 1,
    '__LINE__'         => 1,
    '__PACKAGE__'      => 1,
    'abs'              => 1,
    'accept'           => 1,
    'alarm'            => 1,
    'and'              => 1,
    'atan2'            => 1,
    'bind'             => 1,
    'binmode'          => 1,
    'bless'            => 1,
    'break'            => 1,
    'caller'           => 1,
    'chdir'            => 1,
    'chmod'            => 1,
    'chomp'            => 1,
    'chop'             => 1,
    'chown'            => 1,
    'chr'              => 1,
    'chroot'           => 1,
    'close'            => 1,
    'closedir'         => 1,
    'cmp'              => 1,
    'connect'          => 1,
    'continue'         => 1,
    'cos'              => 1,
    'crypt'            => 1,
    'dbmclose'         => 1,
    'dbmopen'          => 1,
    'default'          => 1,
    'die'              => 1,
    'do'               => 1,
    'dump'             => 1,
    'each'             => 1,
    'endgrent'         => 1,
    'endhostent'       => 1,
    'endnetent'        => 1,
    'endprotoent'      => 1,
    'endpwent'         => 1,
    'endservent'       => 1,
    'eof'              => 1,
    'exec'             => 1,
    'exit'             => 1,
    'exp'              => 1,
    'fc'               => 1,
    'fcntl'            => 1,
    'fileno'           => 1,
    'flock'            => 1,
    'fork'             => 1,
    'formline'         => 1,
    'getc'             => 1,
    'getgrent'         => 1,
    'getgrgid'         => 1,
    'getgrnam'         => 1,
    'gethostbyaddr'    => 1,
    'gethostbyname'    => 1,
    'gethostent'       => 1,
    'getlogin'         => 1,
    'getnetbyaddr'     => 1,
    'getnetbyname'     => 1,
    'getnetent'        => 1,
    'getpeername'      => 1,
    'getpgrp'          => 1,
    'getppid'          => 1,
    'getpriority'      => 1,
    'getprotobyname'   => 1,
    'getprotobynumber' => 1,
    'getprotoent'      => 1,
    'getpwent'         => 1,
    'getpwnam'         => 1,
    'getpwuid'         => 1,
    'getservbyname'    => 1,
    'getservbyport'    => 1,
    'getservent'       => 1,
    'getsockname'      => 1,
    'getsockopt'       => 1,
    'given'            => 1,
    'glob'             => 1,
    'gmtime'           => 1,
    'hex'              => 1,
    'index'            => 1,
    'int'              => 1,
    'ioctl'            => 1,
    'join'             => 1,
    'keys'             => 1,
    'kill'             => 1,
    'lc'               => 1,
    'lcfirst'          => 1,
    'length'           => 1,
    'link'             => 1,
    'listen'           => 1,
    'localtime'        => 1,
    'lock'             => 1,
    'log'              => 1,
    'lstat'            => 1,
    'mkdir'            => 1,
    'msgctl'           => 1,
    'msgget'           => 1,
    'msgrcv'           => 1,
    'msgsnd'           => 1,
    'not'              => 1,
    'oct'              => 1,
    'open'             => 1,
    'opendir'          => 1,
    'or'               => 1,
    'ord'              => 1,
    'pack'             => 1,
    'pipe'             => 1,
    'pop'              => 1,
    'push'             => 1,
    'quotemeta'        => 1,
    'rand'             => 1,
    'read'             => 1,
    'readdir'          => 1,
    'readline'         => 1,
    'readlink'         => 1,
    'readpipe'         => 1,
    'recv'             => 1,
    'ref'              => 1,
    'rename'           => 1,
    'require'          => 1,
    'reset'            => 1,
    'reverse'          => 1,
    'rewinddir'        => 1,
    'rindex'           => 1,
    'rmdir'            => 1,
    'say'              => 1,
    'seek'             => 1,
    'seekdir'          => 1,
    'select'           => 1,
    'semctl'           => 1,
    'semget'           => 1,
    'semop'            => 1,
    'send'             => 1,
    'setgrent'         => 1,
    'sethostent'       => 1,
    'setnetent'        => 1,
    'setpgrp'          => 1,
    'setpriority'      => 1,
    'setprotoent'      => 1,
    'setpwent'         => 1,
    'setservent'       => 1,
    'setsockopt'       => 1,
    'shift'            => 1,
    'shmctl'           => 1,
    'shmget'           => 1,
    'shmread'          => 1,
    'shmwrite'         => 1,
    'shutdown'         => 1,
    'sin'              => 1,
    'sleep'            => 1,
    'socket'           => 1,
    'socketpair'       => 1,
    'splice'           => 1,
    'sprintf'          => 1,
    'sqrt'             => 1,
    'srand'            => 1,
    'stat'             => 1,
    'state'            => 1,
    'substr'           => 1,
    'symlink'          => 1,
    'syscall'          => 1,
    'sysopen'          => 1,
    'sysread'          => 1,
    'sysseek'          => 1,
    'system'           => 1,
    'syswrite'         => 1,
    'tell'             => 1,
    'telldir'          => 1,
    'tie'              => 1,
    'tied'             => 1,
    'time'             => 1,
    'times'            => 1,
    'truncate'         => 1,
    'uc'               => 1,
    'ucfirst'          => 1,
    'umask'            => 1,
    'unlink'           => 1,
    'unpack'           => 1,
    'unshift'          => 1,
    'untie'            => 1,
    'utime'            => 1,
    'values'           => 1,
    'vec'              => 1,
    'wait'             => 1,
    'waitpid'          => 1,
    'wantarray'        => 1,
    'warn'             => 1,
    'when'             => 1,
    'write'            => 1,
    'xor'              => 1,
};

# This list was generated by misc/Perl5/core_global_override.pl
#
# From perlsub:
#
#    The built-ins "do", "require" and "glob" can also be overridden, but due
#    to special magic, their original syntax is preserved, and you don't have
#    to define a prototype for their replacements. (You can't override the "do
#    BLOCK" syntax, though).
#
#    "require" has special additional dark magic: if you invoke your "require"
#    replacement as "require Foo::Bar", it will actually receive the argument
#    "Foo/Bar.pm" in @_. See "require" in perlfunc.
#
#    And, as you'll have noticed from the previous example, if you override
#    "glob", the "<*>" glob operator is overridden as well.
#
#    In a similar fashion, overriding the "readline" function also overrides
#    the equivalent I/O operator "<FILEHANDLE>". Also, overriding "readpipe"
#    also overrides the operators `` and "qx//".
#

our $CORE_GLOBAL_OVERRIDABLE = {
    '__FILE__'         => 1,
    '__LINE__'         => 1,
    '__PACKAGE__'      => 1,
    'abs'              => 1,
    'accept'           => 1,
    'alarm'            => 1,
    'and'              => 1,
    'atan2'            => 1,
    'bind'             => 1,
    'binmode'          => 1,
    'bless'            => 1,
    'caller'           => 1,
    'chdir'            => 1,
    'chmod'            => 1,
    'chomp'            => 1,
    'chop'             => 1,
    'chown'            => 1,
    'chr'              => 1,
    'chroot'           => 1,
    'close'            => 1,
    'closedir'         => 1,
    'cmp'              => 1,
    'connect'          => 1,
    'continue'         => 1,
    'cos'              => 1,
    'crypt'            => 1,
    'dbmclose'         => 1,
    'dbmopen'          => 1,
    'die'              => 1,
    'do'               => 1,
    'dump'             => 1,
    'each'             => 1,
    'endgrent'         => 1,
    'endhostent'       => 1,
    'endnetent'        => 1,
    'endprotoent'      => 1,
    'endpwent'         => 1,
    'endservent'       => 1,
    'eof'              => 1,
    'exec'             => 1,
    'exit'             => 1,
    'exp'              => 1,
    'fcntl'            => 1,
    'fileno'           => 1,
    'flock'            => 1,
    'fork'             => 1,
    'formline'         => 1,
    'getc'             => 1,
    'getgrent'         => 1,
    'getgrgid'         => 1,
    'getgrnam'         => 1,
    'gethostbyaddr'    => 1,
    'gethostbyname'    => 1,
    'gethostent'       => 1,
    'getlogin'         => 1,
    'getnetbyaddr'     => 1,
    'getnetbyname'     => 1,
    'getnetent'        => 1,
    'getpeername'      => 1,
    'getpgrp'          => 1,
    'getppid'          => 1,
    'getpriority'      => 1,
    'getprotobyname'   => 1,
    'getprotobynumber' => 1,
    'getprotoent'      => 1,
    'getpwent'         => 1,
    'getpwnam'         => 1,
    'getpwuid'         => 1,
    'getservbyname'    => 1,
    'getservbyport'    => 1,
    'getservent'       => 1,
    'getsockname'      => 1,
    'getsockopt'       => 1,
    'glob'             => 1,
    'gmtime'           => 1,
    'hex'              => 1,
    'index'            => 1,
    'int'              => 1,
    'ioctl'            => 1,
    'join'             => 1,
    'keys'             => 1,
    'kill'             => 1,
    'lc'               => 1,
    'lcfirst'          => 1,
    'length'           => 1,
    'link'             => 1,
    'listen'           => 1,
    'localtime'        => 1,
    'lock'             => 1,
    'log'              => 1,
    'lstat'            => 1,
    'mkdir'            => 1,
    'msgctl'           => 1,
    'msgget'           => 1,
    'msgrcv'           => 1,
    'msgsnd'           => 1,
    'not'              => 1,
    'oct'              => 1,
    'open'             => 1,
    'opendir'          => 1,
    'or'               => 1,
    'ord'              => 1,
    'pack'             => 1,
    'pipe'             => 1,
    'pop'              => 1,
    'push'             => 1,
    'quotemeta'        => 1,
    'rand'             => 1,
    'read'             => 1,
    'readdir'          => 1,
    'readline'         => 1,
    'readlink'         => 1,
    'readpipe'         => 1,
    'recv'             => 1,
    'ref'              => 1,
    'rename'           => 1,
    'require'          => 1,
    'reset'            => 1,
    'reverse'          => 1,
    'rewinddir'        => 1,
    'rindex'           => 1,
    'rmdir'            => 1,
    'seek'             => 1,
    'seekdir'          => 1,
    'select'           => 1,
    'semctl'           => 1,
    'semget'           => 1,
    'semop'            => 1,
    'send'             => 1,
    'setgrent'         => 1,
    'sethostent'       => 1,
    'setnetent'        => 1,
    'setpgrp'          => 1,
    'setpriority'      => 1,
    'setprotoent'      => 1,
    'setpwent'         => 1,
    'setservent'       => 1,
    'setsockopt'       => 1,
    'shift'            => 1,
    'shmctl'           => 1,
    'shmget'           => 1,
    'shmread'          => 1,
    'shmwrite'         => 1,
    'shutdown'         => 1,
    'sin'              => 1,
    'sleep'            => 1,
    'socket'           => 1,
    'socketpair'       => 1,
    'splice'           => 1,
    'sprintf'          => 1,
    'sqrt'             => 1,
    'srand'            => 1,
    'stat'             => 1,
    'substr'           => 1,
    'symlink'          => 1,
    'syscall'          => 1,
    'sysopen'          => 1,
    'sysread'          => 1,
    'sysseek'          => 1,
    'system'           => 1,
    'syswrite'         => 1,
    'tell'             => 1,
    'telldir'          => 1,
    'tie'              => 1,
    'tied'             => 1,
    'time'             => 1,
    'times'            => 1,
    'truncate'         => 1,
    'uc'               => 1,
    'ucfirst'          => 1,
    'umask'            => 1,
    'unlink'           => 1,
    'unpack'           => 1,
    'unshift'          => 1,
    'untie'            => 1,
    'utime'            => 1,
    'values'           => 1,
    'vec'              => 1,
    'wait'             => 1,
    'waitpid'          => 1,
    'wantarray'        => 1,
    'warn'             => 1,
    'write'            => 1,
    'xor'              => 1,
};

# the CORE prototype list
# obtained with:
# $ perldoc -u PerlFunc | head -n300 | perl -ne ' push @x, /C<([^>]+)/g; END { eval { $p{"CORE::$_"} = prototype("CORE::$_") } for @x; use Data::Dumper; print Dumper \%p } ' > ~/tmp/core.pm

our $CORE_PROTO = {
    'CORE::abs'              => '_',
    'CORE::accept'           => '**',
    'CORE::alarm'            => '_',
    'CORE::atan2'            => '$$',
    'CORE::bind'             => '*$',
    'CORE::binmode'          => '*;$',
    'CORE::bless'            => '$;$',
    # 'CORE::break'            => '',			# feature
    'CORE::caller'           => ';$',
    'CORE::chdir'            => ';$',
    'CORE::chmod'            => '@',
    'CORE::chomp'            => '_',             # original 'undef',
    'CORE::chop'             => '_',             # original 'undef',
    'CORE::chown'            => '@',
    'CORE::chr'              => '_',
    'CORE::chroot'           => '_',
    'CORE::close'            => ';*',
    'CORE::closedir'         => '*',
    'CORE::connect'          => '*$',
    'CORE::continue'         => '',
    'CORE::cos'              => '_',
    'CORE::crypt'            => '$$',
    'CORE::dbmclose'         => '\\%',
    'CORE::dbmopen'          => '\\%$$',
    # 'CORE::default'        => undef,           # depends on use feature "switch"
    'CORE::defined'          => '_',             # original 'undef',
    'CORE::delete'           => '$',             # original 'undef'
    'CORE::die'              => '@',
    'CORE::do'               => undef,
    'CORE::dump'             => '',
    'CORE::each'             => '+',
    'CORE::endgrent'         => '',
    'CORE::endhostent'       => '',
    'CORE::endnetent'        => '',
    'CORE::endprotoent'      => '',
    'CORE::endpwent'         => '',
    'CORE::endservent'       => '',
    'CORE::eof'              => ';*',
    'CORE::eval'             => '_',             # original undef
    'CORE::exec'             => undef,
    'CORE::exists'           => '$',             # original 'undef',
    'CORE::exit'             => ';$',
    'CORE::exp'              => '_',
    # 'CORE::fc'               => '_',           # depends on use feature "fc"
    'CORE::fcntl'            => '*$$',
    'CORE::fileno'           => '*',
    'CORE::flock'            => '*$',
    'CORE::fork'             => '',
    'CORE::format'           => undef,
    'CORE::formline'         => '$@',
    'CORE::getc'             => ';*',
    'CORE::getgrent'         => '',
    'CORE::getgrgid'         => '$',
    'CORE::getgrnam'         => '$',
    'CORE::gethostbyaddr'    => '$$',
    'CORE::gethostbyname'    => '$',
    'CORE::gethostent'       => '',
    'CORE::getlogin'         => '',
    'CORE::getnetbyaddr'     => '$$',
    'CORE::getnetbyname'     => '$',
    'CORE::getnetent'        => '',
    'CORE::getpeername'      => '*',
    'CORE::getpgrp'          => ';$',
    'CORE::getppid'          => '',
    'CORE::getpriority'      => '$$',
    'CORE::getprotobyname'   => '$',
    'CORE::getprotobynumber' => '$',
    'CORE::getprotoent'      => '',
    'CORE::getpwent'         => '',
    'CORE::getpwnam'         => '$',
    'CORE::getpwuid'         => '$',
    'CORE::getservbyname'    => '$$',
    'CORE::getservbyport'    => '$$',
    'CORE::getservent'       => '',
    'CORE::getsockname'      => '*',
    'CORE::getsockopt'       => '*$$',
    'CORE::given'            => undef,
    'CORE::glob'             => undef,
    'CORE::gmtime'           => ';$',
    'CORE::goto'             => undef,
    'CORE::grep'             => undef,
    'CORE::hex'              => '_',
    'CORE::index'            => '$$;$',
    'CORE::int'              => '_',
    'CORE::ioctl'            => '*$$',
    'CORE::join'             => '$@',
    'CORE::keys'             => '+',
    'CORE::kill'             => '@',
    'CORE::last'             => undef,
    'CORE::lc'               => '_',
    'CORE::lcfirst'          => '_',
    'CORE::length'           => '_',
    'CORE::link'             => '$$',
    'CORE::listen'           => '*$',
    'CORE::local'            => undef,
    'CORE::localtime'        => ';$',
    'CORE::lock'             => '\\$',
    'CORE::log'              => '_',
    'CORE::lstat'            => '*',
    'CORE::map'              => undef,
    'CORE::mkdir'            => '_;$',
    'CORE::msgctl'           => '$$$',
    'CORE::msgget'           => '$$',
    'CORE::msgrcv'           => '$$$$$',
    'CORE::msgsnd'           => '$$$',
    'CORE::my'               => undef,
    'CORE::next'             => undef,
    'CORE::no'               => undef,
    'CORE::oct'              => '_',
    'CORE::open'             => '*;$@',
    'CORE::opendir'          => '*$',
    'CORE::ord'              => '_',
    'CORE::our'              => undef,
    'CORE::pack'             => '$@',
    'CORE::pack'             => '$@',
    'CORE::package'          => undef,
    'CORE::pipe'             => '**',
    'CORE::pop'              => ';+',
    'CORE::pos'              => undef,
    'CORE::print'            => undef,
    'CORE::printf'           => '$@',            # original 'undef',
    'CORE::prototype'        => undef,
    'CORE::push'             => '+@',
    'CORE::quotemeta'        => '_',
    'CORE::rand'             => ';$',
    'CORE::read'             => '*\\$$;$',
    'CORE::readdir'          => '*',
    'CORE::readline'         => ';*',
    'CORE::readlink'         => '_',
    'CORE::readpipe'         => '_',
    'CORE::recv'             => '*\\$$$',
    'CORE::redo'             => undef,
    'CORE::ref'              => '_',
    'CORE::rename'           => '$$',
    'CORE::require'          => undef,
    'CORE::reset'            => ';$',
    'CORE::return'           => undef,
    'CORE::reverse'          => '@',
    'CORE::rewinddir'        => '*',
    'CORE::rindex'           => '$$;$',
    'CORE::rmdir'            => '_',
    # 'CORE::say'              => undef,        # depends on feature "say"
    'CORE::scalar'           => undef,
    'CORE::seek'             => '*$$',
    'CORE::seekdir'          => '*$',
    'CORE::select'           => ';*',
    'CORE::semctl'           => '$$$$',
    'CORE::semget'           => '$$$',
    'CORE::semop'            => '$$',
    'CORE::send'             => '*$$;$',
    'CORE::setgrent'         => '',
    'CORE::sethostent'       => '$',
    'CORE::setnetent'        => '$',
    'CORE::setpgrp'          => ';$$',
    'CORE::setpriority'      => '$$$',
    'CORE::setprotoent'      => '$',
    'CORE::setpwent'         => '',
    'CORE::setservent'       => '$',
    'CORE::setsockopt'       => '*$$$',
    'CORE::shift'            => ';+',
    'CORE::shmctl'           => '$$$',
    'CORE::shmget'           => '$$$',
    'CORE::shmread'          => '$$$$',
    'CORE::shmwrite'         => '$$$$',
    'CORE::shutdown'         => '*$',
    'CORE::sin'              => '_',
    'CORE::sleep'            => ';$',
    'CORE::socket'           => '*$$$',
    'CORE::socketpair'       => '**$$$',
    'CORE::sort'             => undef,
    'CORE::splice'           => '+;$$@',
    'CORE::split'            => undef,
    'CORE::sprintf'          => '$@',
    'CORE::sqrt'             => '_',
    'CORE::srand'            => ';$',
    'CORE::stat'             => '*',
    # 'CORE::state'            => undef,		# feature
    'CORE::study'            => undef,
    'CORE::sub'              => undef,
    'CORE::substr'           => '$$;$$',
    'CORE::symlink'          => '$$',
    'CORE::syscall'          => '$@',
    'CORE::sysopen'          => '*$$;$',
    'CORE::sysread'          => '*\\$$;$',
    'CORE::sysseek'          => '*$$',
    'CORE::system'           => undef,
    'CORE::syswrite'         => '*$;$$',
    'CORE::tell'             => ';*',
    'CORE::telldir'          => '*',
    'CORE::tie'              => '\\[$@%]$;@',    # original undef
    'CORE::tied'             => undef,
    'CORE::time'             => '',
    'CORE::times'            => '',
    'CORE::truncate'         => '$$',
    'CORE::uc'               => '_',
    'CORE::ucfirst'          => '_',
    'CORE::umask'            => ';$',
    'CORE::undef'            => ';$',            # original undef
    'CORE::unlink'           => '@',
    'CORE::unpack'           => '$;$',
    'CORE::unshift'          => '+@',
    'CORE::untie'            => undef,
    'CORE::use'              => undef,
    'CORE::utime'            => '@',
    'CORE::values'           => '+',
    'CORE::vec'              => '$$$',
    'CORE::wait'             => '',
    'CORE::waitpid'          => '$$',
    'CORE::wantarray'        => '',
    'CORE::warn'             => '@',
    'CORE::when'             => undef,
    'CORE::write'            => ';*',

    'CORE::m'                => undef,
    'CORE::q'                => undef,
    'CORE::qq'               => undef,
    'CORE::qw'               => undef,
    'CORE::qx'               => undef,
    'CORE::qr'               => undef,
    'CORE::s'                => undef,
    'CORE::tr'               => undef,
    'CORE::y'                => undef,

    'CORE::if'               => undef,
    'CORE::unless'           => undef,
    'CORE::when'             => undef,
    'CORE::for'              => undef,
    'CORE::foreach'          => undef,
    'CORE::while'            => undef,
    'CORE::given'            => undef,

    'CORE::and'              => undef,
    'CORE::or'               => undef,
    'CORE::xor'              => undef,
    'CORE::not'              => undef,
    'CORE::cmp'              => undef,

    'CORE::__FILE__'         => '',
    'CORE::__LINE__'         => '',
    'CORE::__PACKAGE__'      => '',
    'CORE::__END__'          => undef,
    'CORE::__DATA__'         => undef,
};

sub is_core_sub {
    return 1 if $_[0] eq "CORE::fc"        && $^H{feature_fc};
    return 1 if $_[0] eq "CORE::say"       && $^H{feature_say};
    return 1 if $_[0] eq "CORE::given"     && $^H{feature_switch};
    return 1 if $_[0] eq "CORE::when"      && $^H{feature_switch};
    return 1 if $_[0] eq "CORE::default"   && $^H{feature_switch};
    return 1 if $_[0] eq "CORE::break"     && $^H{feature_switch};
    return 1 if $_[0] eq "CORE::state"     && $^H{feature_state};
    return 1 if $_[0] eq "CORE::evalbytes" && $^H{feature_evalbytes};
    return 1 if $_[0] eq "CORE::__SUB__"   && $^H{feature___SUB__};
    exists $Perlito5::CORE_PROTO->{$_[0]}
}
sub get_prototype_core {
    return "_" if $_[0] eq "CORE::fc"    && $^H{feature_fc};
    return ""  if $_[0] eq "CORE::break" && $^H{feature_switch};
    $Perlito5::CORE_PROTO->{$_[0]}
}

# this is the routine executed by statements like 'require 5.20' and 'use v5.20'
sub test_perl_version {
    my $version = shift;
    $version =~ s/^v//;
    if ($version && ord(substr($version,0,1)) < 10) {
        # v-string to string
        my @v = split(//,$version);
        push @v, chr(0) while @v < 3;
        $version = sprintf("%d.%03d%03d", map { ord($_) } @v);
    }
    else {
        my @v = split(/\./,$version);
        $v[1] = $v[1] . '0' while length($v[1]) < 3;
        $version = join('.', @v);
    }
    if ($version gt $]) {
        die "Perl v$version required--this is only v$]";
    }
    return $version;
}

sub get_label {
    'tmp' . $Perlito5::ID++
}

sub overloading_flag {
    my $no_overloading;
    if ($^H & $Perlito5::HINT_NO_AMAGIC) {
        # TODO - check operator bit in $^H{overloading}
        $no_overloading = 1;
    }
    return ( $no_overloading ? ( _no_overloading => 1 ) : () );
}
sub integer_flag {
    my $op = shift;
    my $is_integer;
    if ( ($^H & $Perlito5::INTEGER) && $Perlito5::Integer{$op} ) {
        $is_integer = 1;
    }
    return ( $is_integer ? ( _integer => 1 ) : () );
}

1;          

