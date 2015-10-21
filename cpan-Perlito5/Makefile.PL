use 5.006;
use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME             => 'Perlito5',
    AUTHOR           => q{Flavio S. Glock <fglock@gmail.com>},
    VERSION_FROM     => 'lib/Perlito5.pm',
    # ABSTRACT_FROM    => 'lib/Perlito5.pm',
    LICENSE          => 'artistic_2',
    PL_FILES         => {},
    MIN_PERL_VERSION => 5.006,
    CONFIGURE_REQUIRES => {
        'ExtUtils::MakeMaker' => 0,
    },
    BUILD_REQUIRES => {
        'Test::More' => 0,
    },
    PREREQ_PM => {
        # no prerequisites
    },
    dist  => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean => { FILES => 'Perlito5-*' },
    META_MERGE => {
        'meta-spec' => { version => 2 },
         resources => {
             repository => {
                 type => 'git',
                 url  => 'https://github.com/fglock/Perlito.git',
                 web  => 'https://github.com/fglock/Perlito',
             },
         },
         no_index => {
             directory => [
                'lib/Perlito5X',
                'lib/Perlito5',
                'src5',
             ],
             package => [
                'name',     # ... package name ... - misparse
             ],
         },
    },
);

