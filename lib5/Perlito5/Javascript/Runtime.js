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

if (typeof NAMESPACE !== 'object') {
    NAMESPACE = {};
    CLASS = {};

    var universal = function () {};
    CLASS.UNIVERSAL = new universal();
    CLASS.UNIVERSAL._ref_ = 'UNIVERSAL';
    CLASS.UNIVERSAL.isa = function (o, s) { return s == o._class_._ref_ };

    NAMESPACE.UNIVERSAL = new universal();

    var core = function () {};
    CLASS.CORE = new core();
    CLASS.CORE._ref_ = 'CORE';

    NAMESPACE.CORE = new core();
}

function make_package(pkg_name) {
    if (!CLASS.hasOwnProperty(pkg_name)) {
        var tmp = function () {};
        tmp.prototype = CLASS.UNIVERSAL;
        CLASS[pkg_name] = new tmp();
        CLASS[pkg_name]._ref_ = pkg_name;
        CLASS[pkg_name]._class_ = CLASS[pkg_name];  // XXX memory leak

        var tmp = function () {};
        tmp.prototype = NAMESPACE.CORE;
        NAMESPACE[pkg_name] = new tmp();
    }
}

function make_sub(pkg_name, sub_name, func) {
    NAMESPACE[pkg_name][sub_name] = CLASS[pkg_name][sub_name] = func;
}

if (typeof arguments === 'object') {
    List_ARGV = arguments;
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

make_package('IO');
make_package('Perlito5::Runtime');
make_package('Perlito5::Grammar');

make_sub('IO', 'slurp', function(filename) {
    if (typeof readFile == 'function') {
        return readFile(filename);
    }
    if (typeof read == 'function') {
        // v8
        return read(filename);
    }
    CLASS.CORE.die("IO.slurp() not implemented");
});

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
        case "string": return '"' + CLASS['Perlito5::Runtime'].lisp_escape_string(o) + '"';
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
        return o._class_.bool(o);
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
    return false;
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

make_sub('Perlito5::Grammar', 'word', function(v_grammar, v_str, v_pos) {
    return {
        str: v_str,
        from: v_pos,
        to: v_pos + 1,
        bool: v_str.substr(v_pos, 1).match(/\w/) != null,
        _class_: CLASS['Perlito5::Match']
    };
});

make_sub('Perlito5::Grammar', 'digit', function(v_grammar, v_str, v_pos) {
    return {
        str:  v_str,
        from: v_pos,
        to:   v_pos + 1,
        bool: v_str.substr(v_pos, 1).match(/\d/) != null,
        _class_: CLASS['Perlito5::Match']
    };
});

make_sub('Perlito5::Grammar', 'space', function(v_grammar, v_str, v_pos) {
    return {
        str:  v_str,
        from: v_pos,
        to:   v_pos + 1,
        bool: v_str.substr(v_pos, 1).match(/\s/) != null,
        _class_: CLASS['Perlito5::Match']
    };
});

function perl5_to_js( source ) {
    // say( "source: [" + source + "]" );
    match = CLASS['Perlito5::Grammar'].exp_stmts(CLASS['Perlito5::Grammar'], source, 0);
    ast = match._class_.flat(match);
    var block = {
        stmts:   ast,
        _class_: CLASS['Lit::Block']
    };
    var tmp = {
        block:   block,
        _class_: CLASS.Do
    };
    ast = tmp;
    // CORE.say( "ast: [" + perl(ast) + "]" );
    js_code = ast._class_.emit_javascript(ast);
    // CORE.say( "js-source: [" + js_code + "]" );
    return js_code;
}

