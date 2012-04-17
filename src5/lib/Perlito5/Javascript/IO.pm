use v5;

package Perlito5::Javascript::IO;

sub emit_javascript {

    return <<'EOT';
//
//
// lib/Perlito5/Javascript/IO.js
//
// I/O functions for "Perlito" Perl5-in-Javascript
//
// AUTHORS
//
// Flavio Soibelmann Glock  fglock@gmail.com
//
// COPYRIGHT
//
// Copyright 2009, 2010, 2011, 2012 by Flavio Soibelmann Glock and others.
//
// This program is free software; you can redistribute it and/or modify it
// under the same terms as Perl itself.
//
// See http://www.perl.com/perl/misc/Artistic.html

var isNode = typeof require != "undefined";
if (isNode) {

    var fs = require("fs");

    p5atime = function(s) {
        var stat = fs.statSync(s); return stat["atime"];
    };
    p5mtime = function(s) {
        var stat = fs.statSync(s); return stat["mtime"];
    };
    p5ctime = function(s) {
        var stat = fs.statSync(s); return stat["ctime"];
    };
    p5size = function(s) {
        var stat = fs.statSync(s); return stat["size"];
    };
    p5is_file = function(s) {
        var stat = fs.statSync(s); return stat.isFile() ? 1 : 0;
    };
    p5is_directory = function(s) {
        var stat = fs.statSync(s); return stat.isDirectory() ? 1 : 0;
    };
    p5file_exists = function(s) {
        return p5is_file(s) || p5is_directory(s);
    };

    CORE.chdir = function(List__) {
        try {
            process.chdir(List__[0]);
            return 1;
        }
        catch(err) {
            return '';
        }
    };

}

EOT
} # end of emit_javascript()

1;

