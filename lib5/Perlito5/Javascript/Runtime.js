//
// lib/Perlito/Javascript/Runtime.js
//
// Runtime for "Perlito" Perl5-in-Javascript
//
// AUTHORS
//
// Flavio Soibelmann Glock  fglock@gmail.com
//
// COPYRIGHT
//
// Copyright 2009, 2010, 2011 by Flavio Soibelmann Glock and others.
//
// This program is free software; you can redistribute it and/or modify it
// under the same terms as Perl itself.
//
// See http://www.perl.com/perl/misc/Artistic.html

if (typeof arguments === 'object') {
    List_ARGV = arguments;
}

// call context - method or subroutine
if (typeof CallSub !== 'object') {
    CallSubClass = function() {};
    CallSub = new CallSubClass;
}

function HashRef(o) {
    this._hash_ = o;
    this._ref_ = 'HASH';
    this.bool = function() { return 1 };
}

function ArrayRef(o) {
    this._array_ = o;
    this._ref_ = 'ARRAY';
    this.bool = function() { return 1 };
}

function ScalarRef(o) {
    this._scalar_ = o;
    this._ref_ = 'SCALAR';
    this.bool = function() { return 1 };
}

// namespace CORE
if (typeof CORE !== 'object') {
  CORE = function() {};
  CORE = new CORE;
}

// class IO
if (typeof IO !== 'object') {
    IO = function() {};
    IO = new IO;
}

IO.slurp = function(v_callsub, filename) {
    if (typeof readFile == 'function') {
        return readFile(filename);
    }
    if (typeof read == 'function') {
        // v8
        return read(filename);
    }
    die("IO.slurp() not implemented");
};

// class Perlito5$Runtime
if (typeof Perlito5$Runtime !== 'object') {
    Perlito5$Runtime = function() {};
    Perlito5$Runtime = new Perlito5$Runtime;
}

// class for grammar primitives
if (typeof Perlito5$Grammar !== 'object') {
    Perlito5$Grammar = function() {};
    Perlito5$Grammar = new Perlito5$Grammar;
}

// XXX this doesn't belong here
Array.prototype.grep = function grep(f) {
    var res = new Array()
    for (var i in this) {
        if (bool(f(this[i]))) {
            res.push(this[i])
        }
    }
    return res
}

// XXX Perl6
perl = function(o) {
    if (o == null) {
        return 'undef';
    }
    if (typeof o === 'object' && (o instanceof Array)) {
        var out = [];
        for (var i = 0; i < o.length; i++) {
            out.push(perl(o[i]));
        }
        return "[" + out.join(", ") + "]";
    }
    switch (typeof o) {
        case "string": return '"' + Perlito5$Runtime.lisp_escape_string(o) + '"';
        case "function": return "function";
        case "number": return o;
        case "boolean": return o;
    }
    var out = [];
    for (var i in o) {
        out.push(i + " => " + perl(o[i]));
    }
    return '{' + out.join(", ") + '}';
};

string = function(o) {
    if (o == null) {
        return "";
    }
    if (typeof o === 'object' && (o instanceof Array)) {
        var out = [];
        for (var i = 0; i < o.length; i++) {
            out.push(string(o[i]));
        }
        return out.join(" ");
    }
    if (typeof o.string === 'function') {
        return o.string();
    }
    if (typeof o !== 'string') {
        return "" + o;
    }
    return o;
};

num = function(o) {
    if (o == null) {
        return 0;
    }
    if (typeof o === 'object' && (o instanceof Array)) {
        return o.length;
    }
    if (typeof o.num === 'function') {
        return o.num();
    }
    if (typeof o !== 'number') {
        return parseFloat(string(o));
    }
    return o;
};

add = function(o1, o2) {
    return num(o1) + num(o2)
};

bool = function(o) {
    if (o == null) {
        return o;
    }
    if (typeof o === 'boolean') {
        return o;
    }
    if (typeof o === 'number') {
        return o;
    }
    if (typeof o === 'string') {
        return o != '' && o != '0';
    }
    if (o._class_ && typeof o._class_.bool === 'function') {
        return o._class_.bool.call(o);
    }
    if (typeof o.bool === 'function') {
        return o.bool();
    }
    if (typeof o.length === 'number') {
        return o.length;
    }
    for (var i in o) {
        return true;
    }
    return false;
};

and = function(a, fb) {
    if (bool(a)) {
        return fb();
    }
    return a;
};

or = function(a, fb) {
    if (bool(a)) {
        return a;
    }
    return fb();
};

defined_or = function(a, fb) {
    if (a == null) {
        return fb();
    }
    return a;
};

str_replicate = function(o, n) {
    n = num(n);
    return n ? Array(n + 1).join(o) : "";
};

Perlito5$Grammar.word = function(v_str, v_pos) {
    var tmp = {
        str: v_str,
        from: v_pos,
        to: v_pos + 1,
        bool: v_str.substr(v_pos, 1).match(/\w/) != null
    };
    tmp._class_ = Perlito5$Match;
    return tmp;
};

Perlito5$Grammar.digit = function(v_str, v_pos) {
    var tmp = {
        str:  v_str,
        from: v_pos,
        to:   v_pos + 1,
        bool: v_str.substr(v_pos, 1).match(/\d/) != null
    };
    tmp._class_ = Perlito5$Match;
    return tmp;
};

Perlito5$Grammar.space = function(v_str, v_pos) {
    var tmp = {
        str:  v_str,
        from: v_pos,
        to:   v_pos + 1,
        bool: v_str.substr(v_pos, 1).match(/\s/) != null
    };
    tmp._class_ = Perlito5$Match;
    return tmp;
};

function perl5_to_js( source ) {
    // say( "source: [" + source + "]" );
    match = Perlito5$Grammar.exp_stmts(source, 0);
    ast = match._class_.flat.call(match);
    var block = {stmts: ast};
    block._class_ = Lit$Block;
    var tmp = {block: block};
    tmp._class_ = Do;   
    ast = tmp;
    // CORE.say( "ast: [" + perl(ast) + "]" );
    js_code = ast._class_.emit_javascript.call(ast);
    // CORE.say( "js-source: [" + js_code + "]" );
    return js_code;
}

