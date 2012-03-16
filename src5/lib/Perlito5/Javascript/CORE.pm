use v5;

package Perlito5::Javascript::CORE;

sub emit_javascript {

    return '//
//
// lib/Perlito5/Javascript/CORE.js
//
// CORE functions for "Perlito" Perl5-in-Javascript
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

var isNode = typeof require != "undefined";
if (isNode) {
    CORE.print = function(List__) {
        var i;
        for (i = 0; i < List__.length; i++) {
            var s = p5str(List__[i]);
            process.stdout.write(s);
        }
        return 1;
    }
} else {
    var _print_buf = "";
    CORE.print = function(List__) {
        var i;
        for (i = 0; i < List__.length; i++) {
            var s = p5str(List__[i]);
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
        return 1;
    };
}

CORE.say = function(List__) {
    CORE.print(List__);
    return CORE.print(["\n"]);
};

CORE.die = function(List__) {
    var i;
    var s = "";
    for (i = 0; i < List__.length; i++) {
        s = s + p5str(List__[i]);
    }
    NAMESPACE["main"]["v_@"] = "Died: " + s;
    throw(new p5_error("Died: " + s));
};

CORE.warn = function(List__) {
    var i;
    var s = "";
    for (i = 0; i < List__.length; i++) {
        s = s + p5str(List__[i]);
    }
    CORE.print(["Warning: " + s + "\n"]);
};

CORE.bless = function(List__) {
    var o        = List__[0];
    var pkg_name = List__[1];
    if (typeof pkg_name === "object") {
        // bless {}, Class
        o._class_ = pkg_name;
        return o;
    }
    if (!NAMESPACE.hasOwnProperty(pkg_name)) {
        make_package(pkg_name);
    }
    o._class_ = NAMESPACE[pkg_name];
    return o;
};

CORE.chr = function(List__) {
    return String.fromCharCode(num(List__[0]));
};

CORE.ord = function(List__) {
    return p5str(List__[0]).charCodeAt(0);
};

CORE.oct = function(List__) {
    var v = List__[0];
    if (v.substr(0,2) == "0b") { return parseInt(v.substr(2), 2)  }
    if (v.substr(0,2) == "0x") { return parseInt(v.substr(2), 16) }
    return parseInt(v, 8);
};

CORE.abs   = function(List__) { return Math.abs(List__[0]) };
CORE.exp   = function(List__) { return Math.exp(List__[0]) };
CORE.log   = function(List__) { return Math.log(List__[0]) };
CORE.cos   = function(List__) { return Math.cos(List__[0]) };
CORE.sin   = function(List__) { return Math.sin(List__[0]) };
CORE.sqrt  = function(List__) { return Math.sqrt(List__[0]) };
CORE.atan2 = function(List__) { return Math.atan2(List__[0], List__[1]) };
CORE.int   = function(List__) { return List__[0] > 0 ? Math.floor(List__[0]) : Math.ceil(List__[0]) };

CORE.lc      = function(List__) { return List__[0].toLowerCase() };
CORE.uc      = function(List__) { return List__[0].toUpperCase() };

CORE.lcfirst = function(List__) {
    var s = List__[0];
    var c = s.length > 0 ? s.slice(0,1).toLowerCase() : "";
    s = s.length > 1 ? substr(s, 1) : "";
    return c + s
};

CORE.ucfirst = function(List__) {
    var s = List__[0];
    var c = s.length > 0 ? s.slice(0,1).toUpperCase() : "";
    s = s.length > 1 ? substr(s, 1) : "";
    return c + s
};

CORE.substr = function(List__) {
    var expr        = List__[0];
    var offset      = List__[1];
    var length      = List__[2];
    var replacement = List__[3];
    return p5str(expr).substr(offset, length);
};

CORE.scalar = function(List__) {
    var o = List__[0];
    if (o == null) {
        return 1;
    };
    if (typeof o.scalar === "function") {
        return o.scalar();
    }
    if (typeof o === "object" && (o instanceof Array)) {
        return o.length;
    }
    switch (typeof o) {
        case "string":   return o;
        case "function": return o;
        case "number":   return o;
        case "boolean":  return o;
    }
    var l = 0;
    for (var i in o) {
        l++;
    }
    return l;
};

CORE.values = function(List__) {
    var o = List__[0];
    if (o == null) {
        return [];
    };
    if (typeof o.values === "function") {
        return o.values();
    }
    var out = [];
    for (var i in o) {
        out.push(o[i]);
    }
    return out;
};

CORE.keys = function(List__) {
    var o = List__[0];
    if (o == null) {
        return [];
    }
    if (typeof o.keys === "function") {
        return o.keys();
    }
    var out = [];
    for (var i in o) {
        out.push(i);
    }
    return out;
};

CORE.reverse = function(List__) {
    var o = List__[0];
    if (o == null) {
        return "";
    }
    if (typeof o === "string") {
        return o.split("").reverse().join("")
    }
    var out = [];
    for(var i = 0; i < o.length; i++) {
        out.unshift(o[i]);
    }
    return out;
};

CORE.splice = function(List__) {
    var array  = List__.shift();
    // CORE.say([ array ]);
    var offset = num(List__.shift());
    var limit  = List__.length ? num(List__.shift()) : (array.length + 1);

    if (limit < 0) {
        limit = array.length + limit - 1;
    }

    var list   = interpolate_array(offset, limit, List__);
    out = array.splice.apply(array, list);
    // CORE.say([ CORE.join([":",array]), " ofs=", offset, " lim=", limit, " list=", list, " out=", CORE.join([":",out])  ]);
    return out;
};

CORE.pop = function(List__) {
    var o = List__[0];
    if (o.length == null) {
        return null;
    }
    return o.pop();
};

CORE.shift = function(List__) {
    var o = List__[0];
    if (o.length == null) {
        return null;
    }
    return o.shift();
};

CORE.push = function(List__) {
    var o = List__[0];
    var v = List__[1];
    for(var i = 0; i < v.length; i++) {
        o.push(v[i]);
    }
    return o.length;
};

CORE.unshift = function(List__) {
    var o = List__[0];
    var v = List__[1];
    for(var i = v.length-1; i >= 0; i--) {
        o.unshift(v[i]);
    }
    return o.length;
};

CORE.join = function(List__) {
    var s = List__[0];
    var o = List__[1];
    return o.join(s);
};

CORE.index = function(List__) {
    var o = List__[0];
    var s = List__[1];
    try {
        return o.indexOf(s);
    }
    catch(err) {
        return -1;
    }
};

CORE.length = function(List__) {
    var o = List__[0];
    if (typeof o.string === "function") {
        return o.string().length;
    }
    return o.length;
};

CORE.ref = function(List__) {
    var o = List__[0];
    if (o == null) {
        return "";
    }
    if (o._class_ && typeof o._class_._ref_ === "string") {
        // blessed reference
        return o._class_._ref_;
    }
    if (typeof o._ref_ === "string") {
        // un-blessed reference
        return o._ref_;
    }
    if (typeof o === "function") {
        return "CODE";
    }
    return "";
};

CORE.split = function(List__) {
    var pattern = List__[0];
    var s       = List__[1];
    var limit   = List__[2];
    if (typeof pattern === "string") {
        if (pattern == " ") {
            var res = [];
            for (var i_ = 0, a_ = s.split(/(?: |\n)+/); i_ < a_.length ; i_++) {
                if (a_[i_] != "") {
                    res.push(a_[i_])
                }
            }
            return res;
        }
    }
    CORE.die(["not implemented"]);
};

CORE.prototype = function(List__, data) {
    var name = List__[0];
    // TODO - fully qualify "name" using information from "data"
    // XXX - lookup in CORE::GLOBAL?
    NAMESPACE["Perlito5"].v_PROTO._hash_[name] || NAMESPACE["Perlito5"].v_CORE_PROTO._hash_[name]
};

';
} # end of emit_javascript()

1;

