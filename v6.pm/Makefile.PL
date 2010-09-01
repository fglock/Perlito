use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'v6',
    AUTHOR              => q{Flávio S. Glock <fglock@gmail.com>},
    VERSION_FROM        => 'lib/v6.pm',
    ABSTRACT_FROM       => 'lib/v6.pm',
    ($ExtUtils::MakeMaker::VERSION >= 6.3002
      ? ('LICENSE'=> 'perl')
      : ()),
    PL_FILES            => {},
    EXE_FILES           => [qw(scripts/perlito)],
    PREREQ_PM => {
        'Filter::Util::Call' => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'v6-*' },
);
