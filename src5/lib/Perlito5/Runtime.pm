package Perlito5;
use Perlito5::Grammar::Scope;
use strict;

$^O = 'perlito5'    unless defined $^O;
$/  = chr(10)       unless defined $/;
$"  = ' '           unless defined $";
$,  = undef         unless defined $,;
$!  = ''            unless defined $!;
$;  = chr(28)       unless defined $;;
$?  = 0             unless defined $?;
# $[  = 0             unless defined $[;    # "assignment to $[ is deprecated"
$]  = '5.022000'    unless $];      # $] is defined(), but ${"main::]"} is not
$^V = bless( { 'original' => 'v5.22.0',
               'qv'       => 1,
               'version'  => [ 5, 22, 0 ]
             }, 'version' )
                    unless defined $^V;

our $EXPAND_USE   = 1;
our $EMIT_USE     = 0;
our $STRICT       = 0;
our $WARNINGS     = 0;
our $UTF8         = 0;
our $BYTES        = 0;
our @CALLER       = ();
our %DATA_SECTION = ();   # contents of the __DATA__ sections per package
our $PKG_NAME     = '';   # current package being compiled
our $LINE_NUMBER  = 0;    # current line number being compiled
our $FILE_NAME    = '';   # current file name being compiled

# information about the current compilation process
our $GLOBAL       = {};
our $BASE_SCOPE   = Perlito5::Grammar::Scope->new_base_scope();
our $SCOPE        = $BASE_SCOPE;    # information about the current block being compiled
our $SCOPE_DEPTH  = 0;
our @SCOPE_STMT      = ();
our @END_BLOCK       = ();    # END block LIFO - array of subs
our @INIT_BLOCK      = ();    # INIT block FIFO - array of subs
our @CHECK_BLOCK     = ();    # CHECK block LIFO - array of subs
our @UNITCHECK_BLOCK = ();    # UNITCHECK block LIFO - array of subs
our %BEGIN_SCRATCHPAD = (); # list of "my" variables captured in BEGIN blocks
our $PROTO           = {};

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

# These builtins can be user-redefined using plain subs.
# No other builtins can be redefined (the ones in $Perlito5::CORE_PROTO).
# The other builtins can still be used for method names.

our $CORE_OVERRIDABLE = {
          'say'     => 1,
          'break'   => 1,
          'given'   => 1,
          'when'    => 1,
          'default' => 1,
          'state'   => 1,
          'lock'    => 1,
};

# the CORE prototype list
# obtained with:
# $ perldoc -u PerlFunc | head -n300 | perl -ne ' push @x, /C<([^>]+)/g; END { eval { $p{"CORE::$_"} = prototype("CORE::$_") } for @x; use Data::Dumper; print Dumper \%p } ' > ~/tmp/core.pm

our $CORE_PROTO = {
          'CORE::shutdown' => '*$',
          'CORE::chop' => '_',      # original 'undef',
          'CORE::lstat' => '*',
          'CORE::rename' => '$$',
          'CORE::lock' => '\\$',
          'CORE::rand' => ';$',
          'CORE::gmtime' => ';$',
          'CORE::gethostbyname' => '$',
          'CORE::each' => '+',
          'CORE::ref' => '_',
          'CORE::syswrite' => '*$;$$',
          'CORE::msgctl' => '$$$',
          'CORE::getnetbyname' => '$',
          'CORE::write' => ';*',
          'CORE::alarm' => '_',
          'CORE::print' => undef,
          'CORE::getnetent' => '',
          'CORE::semget' => '$$$',
          'CORE::use' => undef,
          'CORE::abs' => '_',
          'CORE::break' => '',
          'CORE::undef' => ';$',        # original undef
          'CORE::no' => undef,
          'CORE::eval' => '_',          # original undef
          'CORE::split' => undef,
          'CORE::localtime' => ';$',
          'CORE::sort' => undef,
          'CORE::chown' => '@',
          'CORE::endpwent' => '',
          'CORE::getpwent' => '',
          'CORE::pos' => undef,
          'CORE::lcfirst' => '_',
          'CORE::kill' => '@',
          'CORE::send' => '*$$;$',
          'CORE::endprotoent' => '',
          'CORE::semctl' => '$$$$',
          'CORE::waitpid' => '$$',
          'CORE::utime' => '@',
          'CORE::dbmclose' => '\\%',
          'CORE::getpwnam' => '$',
          'CORE::substr' => '$$;$$',
          'CORE::listen' => '*$',
          'CORE::getprotoent' => '',
          'CORE::shmget' => '$$$',
          'CORE::our' => undef,
          'CORE::readlink' => '_',
          'CORE::shmwrite' => '$$$$',
          'CORE::times' => '',
          'CORE::package' => undef,
          'CORE::map' => undef,
          'CORE::join' => '$@',
          'CORE::rmdir' => '_',
          'CORE::shmread' => '$$$$',
          'CORE::uc' => '_',
          'CORE::bless' => '$;$',
          'CORE::closedir' => '*',
          'CORE::getppid' => '',
          'CORE::tie' => '\\[$@%]$;@',     # original undef
          'CORE::readdir' => '*',
          'CORE::gethostent' => '',
          'CORE::getlogin' => '',
          'CORE::last' => undef,
          'CORE::gethostbyaddr' => '$$',
          'CORE::accept' => '**',
          'CORE::log' => '_',
          'CORE::tell' => ';*',
          'CORE::readline' => ';*',
          'CORE::tied' => undef,
          'CORE::socket' => '*$$$',
          'CORE::umask' => ';$',
          'CORE::sysread' => '*\\$$;$',
          'CORE::syscall' => '$@',
          'CORE::quotemeta' => '_',
          'CORE::dump' => '',
          'CORE::opendir' => '*$',
          'CORE::untie' => undef,
          'CORE::truncate' => '$$',
          'CORE::select' => ';*',
          'CORE::sleep' => ';$',
          'CORE::seek' => '*$$',
          'CORE::read' => '*\\$$;$',
          'CORE::rewinddir' => '*',
          'CORE::scalar' => undef,
          'CORE::wantarray' => '',
          'CORE::oct' => '_',
          'CORE::bind' => '*$',
          'CORE::stat' => '*',
          'CORE::sqrt' => '_',
          'CORE::getc' => ';*',
          'CORE::fileno' => '*',
          'CORE::getpeername' => '*',
          'CORE::sin' => '_',
          'CORE::getnetbyaddr' => '$$',
          'CORE::grep' => undef,
          'CORE::setservent' => '$',
          'CORE::sub' => undef,
          'CORE::shmctl' => '$$$',
          'CORE::study' => undef,
          'CORE::msgrcv' => '$$$$$',
          'CORE::setsockopt' => '*$$$',
          'CORE::int' => '_',
          'CORE::pop' => ';+',
          'CORE::link' => '$$',
          'CORE::exec' => undef,
          'CORE::setpwent' => '',
          'CORE::mkdir' => '_;$',
          'CORE::sysseek' => '*$$',
          'CORE::endservent' => '',
          'CORE::chr' => '_',
          'CORE::when' => undef,
          'CORE::getpwuid' => '$',
          'CORE::setprotoent' => '$',
          'CORE::reverse' => '@',
          'CORE::say' => undef,
          'CORE::goto' => undef,
          'CORE::getgrent' => '',
          'CORE::endnetent' => '',
          'CORE::hex' => '_',
          'CORE::binmode' => '*;$',
          'CORE::formline' => '$@',
          'CORE::getgrnam' => '$',
          'CORE::ucfirst' => '_',
          'CORE::chdir' => ';$',
          'CORE::setnetent' => '$',
          'CORE::splice' => '+;$$@',
          'CORE::unlink' => '@',
          'CORE::time' => '',
          'CORE::push' => '+@',
          'CORE::exit' => ';$',
          'CORE::endgrent' => '',
          'CORE::unshift' => '+@',
          'CORE::local' => undef,
          'CORE::my' => undef,
          'CORE::cos' => '_',
          'CORE::redo' => undef,
          'CORE::warn' => '@',
          'CORE::getsockname' => '*',
          'CORE::pipe' => '**',
          'CORE::sprintf' => '$@',
          'CORE::open' => '*;$@',
          'CORE::setpgrp' => ';$$',
          'CORE::exp' => '_',
          'CORE::seekdir' => '*$',
          'CORE::getservbyport' => '$$',
          'CORE::given' => undef,
          'CORE::pack' => '$@',
          'CORE::msgget' => '$$',
          'CORE::rindex' => '$$;$',
          'CORE::srand' => ';$',
          'CORE::telldir' => '*',
          'CORE::connect' => '*$',
          'CORE::getprotobyname' => '$',
          'CORE::msgsnd' => '$$$',
          'CORE::length' => '_',
          'CORE::state' => undef,
          'CORE::die' => '@',
          'CORE::delete' => '$',    # original 'undef'
          'CORE::getservent' => '',
          'CORE::getservbyname' => '$$',
          'CORE::setpriority' => '$$$',
          'CORE::lc' => '_',
          'CORE::fc' => '_',
          'CORE::pack' => '$@',
          'CORE::fcntl' => '*$$',
          'CORE::chroot' => '_',
          'CORE::recv' => '*\\$$$',
          'CORE::dbmopen' => '\\%$$',
          'CORE::socketpair' => '**$$$',
          'CORE::vec' => '$$$',
          'CORE::system' => undef,
          'CORE::defined' => '_',    # original 'undef',
          'CORE::index' => '$$;$',
          'CORE::caller' => ';$',
          'CORE::close' => ';*',
          'CORE::atan2' => '$$',
          'CORE::semop' => '$$',
          'CORE::unpack' => '$;$',
          'CORE::ord' => '_',
          'CORE::chmod' => '@',
          'CORE::prototype' => undef,
          'CORE::getprotobynumber' => '$',
          'CORE::values' => '+',
          'CORE::chomp' => '_',     # original 'undef',
          'CORE::ioctl' => '*$$',
          'CORE::eof' => ';*',
          'CORE::crypt' => '$$',
          'CORE::do' => undef,
          'CORE::flock' => '*$',
          'CORE::wait' => '',
          'CORE::sethostent' => '$',
          'CORE::return' => undef,
          'CORE::getsockopt' => '*$$',
          'CORE::fork' => '',
          'CORE::require' => undef,
          'CORE::format' => undef,
          'CORE::readpipe' => '_',
          'CORE::endhostent' => '',
          'CORE::getpgrp' => ';$',
          'CORE::setgrent' => '',
          'CORE::keys' => '+',
          'CORE::glob' => undef,
          'CORE::getpriority' => '$$',
          'CORE::reset' => ';$',
          'CORE::sysopen' => '*$$;$',
          'CORE::continue' => '',
          'CORE::next' => undef,
          'CORE::getgrgid' => '$',
          'CORE::default' => undef,
          'CORE::shift' => ';+',
          'CORE::symlink' => '$$',
          'CORE::exists' => '$',      # original 'undef',
          'CORE::printf' => '$@',      # original 'undef',

          'CORE::m'      => undef, 
          'CORE::q'      => undef,
          'CORE::qq'     => undef,
          'CORE::qw'     => undef,
          'CORE::qx'     => undef,
          'CORE::qr'     => undef,
          'CORE::s'      => undef,
          'CORE::tr'     => undef,
          'CORE::y'      => undef,

          'CORE::if'     => undef, 
          'CORE::unless' => undef,
          'CORE::when'   => undef,
          'CORE::for'    => undef,
          'CORE::foreach'=> undef,
          'CORE::while'  => undef,
          'CORE::given'  => undef,

          'CORE::and'    => undef,
          'CORE::or'     => undef,
          'CORE::xor'    => undef,
          'CORE::not'    => undef,
          'CORE::cmp'    => undef,

          'CORE::__FILE__' => '',
          'CORE::__LINE__' => '',
        };  

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
}

sub get_label {
    'tmp' . $Perlito5::ID++
}

1;          

