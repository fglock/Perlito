//
// lib/Perlito/Javascript/CORE.js
//
// CORE functions for "Perlito" Perlito5-in-Javascript
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

var CORE = NAMESPACE.CORE;

var _print_buf = "";
CORE.print = function() {
    var List__ = Array.prototype.slice.call(arguments);
    var i;
    for (i = 0; i < List__.length; i++) {
        var s = string(List__[i]);
        if (s.substr(s.length - 2, 2) == "\n") {
            print(_print_buf + s.substr(0, s.length - 2));
            _print_buf = "";
        }
        else if (s.substr(s.length - 1, 1) == "\n") {
            print(_print_buf + s.substr(0, s.length - 1));
            _print_buf = "";
        }
        else {
            _print_buf = _print_buf + s;
        }
    }
    return true;
};

CORE.say = function() {
    var List__ = Array.prototype.slice.call(arguments);
    var i;
    for (i = 0; i < List__.length; i++) {
        CORE.print(List__[i]);
    }
    return CORE.print("\n");
};

CORE.die = function() {
    var List__ = Array.prototype.slice.call(arguments);
    var i;
    var s = '';
    for (i = 0; i < List__.length; i++) {
        s = s + string(List__[i]);
    }
    throw(new Error("Died: " + s));
};

CORE.warn = function() {
    var List__ = Array.prototype.slice.call(arguments);
    var i;
    var s = '';
    for (i = 0; i < List__.length; i++) {
        s = s + string(List__[i]);
    }
    CORE.print("Warning: " + s + "\n");
};

CORE.bless = function(o, pkg_name) {
    if (typeof pkg_name === 'object') {
        // bless {}, Class
        o._class_ = pkg_name;
        return o;
    }
    if (!CLASS.hasOwnProperty(pkg_name)) {
        make_package(pkg_name);
    }
    o._class_ = CLASS[pkg_name];
    return o;
};

CORE.chr = function(o) {
    return String.fromCharCode(num(o));
};

CORE.ord = function(o) {
    return string(o).charCodeAt(0);
};

CORE.scalar = function() {
    var List__ = Array.prototype.slice.call(arguments);
    var o = List__[0];
    if (o == null) {
        return 1;
    };
    if (typeof o.scalar === 'function') {
        return o.scalar();
    }
    if (typeof o === 'object' && (o instanceof Array)) {
        return o.length;
    }
    switch (typeof o) {
        case "string": return 1;
        case "function": return 1;
        case "number": return 1;
        case "boolean": return 1;
    }
    var l = 0;
    for (var i in o) {
        l++;
    }
    return l;
};

CORE.values = function() {
    var List__ = Array.prototype.slice.call(arguments);
    var o = List__[0];
    if (o == null) {
        return [];
    };
    if (typeof o.values === 'function') {
        return o.values();
    }
    if (typeof o === 'object' && (o instanceof Array)) {
        return o;
    }
    switch (typeof o) {
        case "string": return [o];
        case "function": return [o];
        case "number": return [o];
        case "boolean": return [o];
    }
    var out = [];
    for (var i in o) {
        out.push(o[i]);
    }
    return out;
};

CORE.keys = function() {
    var List__ = Array.prototype.slice.call(arguments);
    var o = List__[0];
    if (o == null) {
        return [];
    }
    if (typeof o.keys === 'function') {
        return o.keys();
    }
    var out = [];
    if (typeof o === 'object' && (o instanceof Array)) {
        var count = 0;
        for (var i in o) {
            out.push(count);
            count++;
        }
        return out;
    }
    for (var i in o) {
        out.push(i);
    }
    return out;
};

CORE.pop = function() {
    var List__ = Array.prototype.slice.call(arguments);
    var o = List__[0];
    if (o.length == null) {
        return null;
    }
    return o.pop();
};

CORE.shift = function() {
    var List__ = Array.prototype.slice.call(arguments);
    var o = List__[0];
    if (o.length == null) {
        return null;
    }
    return o.shift();
};

CORE.push = function() {
    var List__ = Array.prototype.slice.call(arguments);
    var o = List__[0];
    var v = List__[1];
    return o.push(v);
};

CORE.unshift = function() {
    var List__ = Array.prototype.slice.call(arguments);
    var o = List__[0];
    var v = List__[1];
    return o.unshift(v);
};

CORE.join = function() {
    var List__ = Array.prototype.slice.call(arguments);
    var s = List__[0];
    var o = List__[1];
    return o.join(s);
};

CORE.index = function() {
    var List__ = Array.prototype.slice.call(arguments);
    var o = List__[0];
    var s = List__[1];
    try {
        return o.indexOf(s);
    }
    catch(err) {
        return -1;
    }
};

CORE.length = function() {
    var List__ = Array.prototype.slice.call(arguments);
    var o = List__[0];
    if (typeof o.string === 'function') {
        return o.string().length;
    }
    return o.length;
};

CORE.ref = function(o) {
    var List__ = Array.prototype.slice.call(arguments);
    var o = List__[0];
    if (o == null) {
        return '';
    }
    if (o._class_ && typeof o._class_._ref_ === 'string') {
        // blessed reference
        return o._class_._ref_;
    }
    if (typeof o._ref_ === 'string') {
        // un-blessed reference
        return o._ref_;
    }
    if (typeof o === 'object' && (o instanceof Array)) {
        return 'ARRAY';
    }
    if (typeof o === 'function') {
        return 'CODE';
    }
    return '';
};

