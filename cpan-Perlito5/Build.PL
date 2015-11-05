#!/usr/bin/perl

use strict;
use warnings;
 
use Module::Build;
 
my $build = Module::Build->new
  (
   module_name        => 'Perlito5',
   dist_version_from  => 'lib/Perlito5.pm',
   license            => 'perl',
   build_requires     => {
                          'Test::More'    => '0.01',
                          'Module::Build' => '0.20',
                         },
   requires           => {
                           'perl' => 5.014,
                         },
   script_files       => [ 'bin/perlito5' ],
   dist_abstract      => 'a Perl5 compiler',
  );
 
$build->create_build_script;
 

