#!/usr/bin/perl

use strict;
use warnings;
use File::Copy;

# cat files
print "Generating one big LISP file...\n";
my @files = (
	'liblisp/MiniPerl6/Lisp/Runtime.lisp',
	'liblisp/MiniPerl6/Lisp/Prelude.lisp',
	'liblisp/MiniPerl6/Lisp/Emitter.lisp',
	'liblisp/MiniPerl6/Grammar.lisp',
	'liblisp/MiniPerl6/Grammar/Control.lisp',
	'liblisp/MiniPerl6/Grammar/Mapping.lisp',
	'liblisp/MiniPerl6/Grammar/Regex.lisp',
	'liblisp/MiniPerl6/Emitter/Token.lisp',
	'util-lisp/create-core-image.lisp',
);

my $outfilename = "mp6-lisp-create-core-image.lisp"; 
open OUT, '>', $outfilename
		or die "Cannot create $outfilename\n";
foreach my $file (@files) {
	open FILE, $file
		or die "Cannot add $file\n";
	local $/ = undef;
	print OUT <FILE>;
	close FILE;
}
close OUT;


# compile...
my $sbcl_log = "sbcl-log.txt";

print "Compiling using sbcl. Please check $sbcl_log for more information.\n";
my $mp6_lisp = 'mp6-lisp';
unlink $mp6_lisp;
if($^O eq 'MSWin32') {
	unlink "$mp6_lisp.exe";
}
system("sbcl --load mp6-lisp-create-core-image.lisp >$sbcl_log 2>&1");

# rename executable if on win32
if(-f $mp6_lisp) {
	if($^O eq 'MSWin32') {
		print "On win32 you need to rename to exe!\n";
		move($mp6_lisp,"$mp6_lisp.exe")
			or die "Cannot move $mp6_lisp to $mp6_lisp.exe on win32";
	}
} else {
	die "sbcl compilation failure. please check $sbcl_log\n";
}

print "\n\nYou can try it now using:\n\n";
if($^O eq 'MSWin32') {
	print qq%$mp6_lisp -e " class Main { say \\"hello, World!\\" } " 2>error.txt\n%;
} else {
	print qq%./$mp6_lisp -e ' class Main { say "hello, World!" } ' 2>error.txt\n%;
}


__END__

=head1 NAME

create-mp6-list.pl - create mp6-list executable

=head1 SYNOPSIS

	perl create-mp6-list.pl 

=head1 DESCRIPTION

A simple Perl script to automate generation of mp6-lisp executable this should 
work on win32 and linux

=head1 AUTHOR

Ahmad M. Zawawi C<< <ahmad.zawawi at gmail.com> >>

=head1 COPYRIGHT AND LICENSE

Copyright 2009 C<< <ahmad.zawawi at gmail.com> >>

This program is free software; you can redistribute it and/or
modify it under the same terms as Perl 5 itself.
