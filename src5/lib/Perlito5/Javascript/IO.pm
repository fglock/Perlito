use v5;

package Perlito5::Javascript::IO;

sub emit_javascript {

    return '//
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

    p5atime = function(List__) {
        var stat = fs.statSync(p5str(List__[0]));
        return stat["atime"];
    }

}

';
} # end of emit_javascript()

1;

