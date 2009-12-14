// MiniPerl6::Grammar
// lib/MiniPerl6/Javascript/Regex.go
//
// Runtime for "Perlito" MiniPerl6-in-Go
//
// AUTHORS
//
// Flavio Soibelmann Glock  fglock@gmail.com
//
// SEE ALSO
//
// The Perl 6 homepage at http://dev.perl.org/perl6
// The Pugs homepage at http://pugscode.org/
//
// COPYRIGHT
//
// Copyright 2009 by Flavio Soibelmann Glock and others.
//
// This program is free software; you can redistribute it and/or modify it
// under the same terms as Perl itself.
//
// See http://www.perl.com/perl/misc/Artistic.html

// Regex primitives

func (v_grammar *MiniPerl6__Grammar) f_is_newline(v Capture) Any {
    v_str := v.p[0];
    v_pos := v.p[1];
    var s1 = v_str.Str().s;
    var i1 = v_pos.Int().i;
    var b1 = ( s1[i1:i1] == "\n" );
    var m Any = new(MiniPerl6__Match);
    m.v_str.(bind_er).Bind(v_str);
    m.v_from.(bind_er).Bind(v_pos);
    m.v_to.(bind_er).Bind(v_pos);
    m.v_bool.(bind_er).Bind(Bool{i: b1});                
    return m;
}

// end: Regex primitives

