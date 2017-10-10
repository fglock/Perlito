# This file was created by configpm when Perl was built. Any changes
# made to this file will be lost the next time perl is built.

# for a description of the variables, please have a look at the
# Glossary file, as written in the Porting folder, or use the url:
# http://perl5.git.perl.org/perl.git/blob/HEAD:/Porting/Glossary

package Config;
use strict;
use warnings;
use vars '%Config', '$VERSION';

$VERSION = "5.022000";

# Skip @Config::EXPORT because it only contains %Config, which we special
# case below as it's not a function. @Config::EXPORT won't change in the
# lifetime of Perl 5.
my %Export_Cache = (myconfig => 1, config_sh => 1, config_vars => 1,
		    config_re => 1, compile_date => 1, local_patches => 1,
		    bincompat_options => 1, non_bincompat_options => 1,
		    header_files => 1);

@Config::EXPORT = qw(%Config);
@Config::EXPORT_OK = keys %Export_Cache;

# Need to stub all the functions to make code such as print Config::config_sh
# keep working

sub bincompat_options;
sub compile_date;
sub config_re;
sub config_sh;
sub config_vars;
sub header_files;
sub local_patches;
sub myconfig;
sub non_bincompat_options;

# Define our own import method to avoid pulling in the full Exporter:
sub import {
    shift;
    @_ = @Config::EXPORT unless @_;

    my @funcs = grep $_ ne '%Config', @_;
    my $export_Config = @funcs < @_ ? 1 : 0;

    no strict 'refs';
    my $callpkg = caller(0);
    foreach my $func (@funcs) {
	die qq{"$func" is not exported by the Config module\n}
	    unless $Export_Cache{$func};
	*{$callpkg.'::'.$func} = \&{$func};
    }

    *{"$callpkg\::Config"} = \%Config if $export_Config;
    return;
}

# die "$0: Perl lib version (5.22.0) doesn't match executable '$^X' version ($])"
#     unless $^V;
# 
# $^V eq 5.22.0
#     or die sprintf "%s: Perl lib version (5.22.0) doesn't match executable '$^X' version (%vd)", $0, $^V;


%Config = (
    archlibexp => '/usr/local/lib/perl5/5.22.0/darwin-2level',
    archname => 'darwin-2level',
    cc => 'cc',
    d_readlink => 'define',
    d_symlink => 'define',
    dlext => 'bundle',
    dlsrc => 'dl_dlopen.xs',
    dont_use_nlink => undef,
    exe_ext => '',
    inc_version_list => '5.20.0',
    intsize => '4',
    ivsize  => '4',
    ldlibpthname => 'DYLD_LIBRARY_PATH',
    libpth => '/usr/local/lib /usr/bin/../lib/clang/4.2/lib /usr/lib /opt/local/lib',
    osname => 'darwin',
    osvers => '12.6.0',
    path_sep => ':',
    privlibexp => '/usr/local/lib/perl5/5.22.0',
    scriptdir => '/usr/local/bin',
    sitearchexp => '/usr/local/lib/perl5/site_perl/5.22.0/darwin-2level',
    sitelibexp => '/usr/local/lib/perl5/site_perl/5.22.0',
    so => 'dylib',
    useithreads => undef,
    usevendorprefix => undef,
    version => '5.22.0',
);

