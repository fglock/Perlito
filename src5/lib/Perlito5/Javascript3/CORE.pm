use v5;

package Perlito5::Javascript3::CORE;

sub emit_javascript3 {

    return <<'EOT'
//
//
// lib/Perlito5/Javascript3/CORE.js
//
// CORE functions for "Perlito" Perl5-in-Javascript3
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

var CORE = p5pkg.CORE;

var isNode = typeof require != "undefined";
if (isNode) {
    CORE.print = function(List__) {
        var i;
        for (i = 0; i < List__.length; i++) {
            process.stdout.write(p5str(List__[i]));
        }
        return 1;
    }
} else {
    CORE.print = function(List__) {
        var i;
        for (i = 0; i < List__.length; i++) {
            write(p5str(List__[i]));
        }
        return 1;
    };
}

CORE.say = function(List__) {
    CORE.print(List__);
    return CORE.print(["\n"]);
};

CORE.die = function(List__) {
    var o = List__[0]._array_;   // prototype is '@'
    var i;
    var s = "";
    for (i = 0; i < o.length; i++) {
        s = s + p5str(o[i]);
    }
    try {
        s = s + "\n" + new Error().stack;
    }
    catch(err) { }
    p5pkg["main"]["v_@"].assign("Died: " + s);
    throw(new p5_error("die", "Died: " + s));
};

CORE.warn = function(List__) {
    var o = List__[0]._array_;   // prototype is '@'
    var i;
    var s = "";
    for (i = 0; i < o.length; i++) {
        s = s + p5str(o[i]);
    }
    try {
        s = s + "\n" + new Error().stack;
    }
    catch(err) { }
    CORE.print(["Warning: " + s + "\n"]);
};

CORE.bless = function(List__) {
    var o        = List__[0];
    var pkg_name = List__[1];
    if (o instanceof p5Scalar) {
        o = o.FETCH();
    }
    if (pkg_name instanceof p5Scalar) {
        pkg_name = pkg_name.FETCH();
    }
    if (typeof pkg_name === "object") {
        // bless {}, Class
        o._class_ = pkg_name;
        return o;
    }
    if (!p5pkg.hasOwnProperty(pkg_name)) {
        p5make_package(pkg_name);
    }
    o._class_ = p5pkg[pkg_name];
    return o;
};

CORE.caller = function(List__, p5want) {
    if (p5want) {
        return p5pkg["Perlito5"]["CALLER"][0];
    }
    return p5pkg["Perlito5"]["CALLER"][0][0];
};

CORE.chr = function(List__) {
    var v = p5num(List__[0]);
    return String.fromCharCode(v >= 0 ? v : 65533);
};

CORE.ord = function(List__) {
    return p5str(List__[0]).charCodeAt(0);
};

CORE.oct = function(List__) {
    var v = p5str(List__[0]);
    var b = v.substr(0,2);
    v = v.replace("_", "");
    if (b == "0b" || b == "0B") { return parseInt(v.substr(2), 2)  }
    if (b == "0x" || b == "0X") { return parseInt(v.substr(2), 16) }
    return parseInt(v, 8);
};

CORE.abs   = function(List__) { return Math.abs(p5num(List__[0])) };
CORE.exp   = function(List__) { return Math.exp(p5num(List__[0])) };
CORE.log   = function(List__) { return Math.log(p5num(List__[0])) };
CORE.cos   = function(List__) { return Math.cos(p5num(List__[0])) };
CORE.sin   = function(List__) { return Math.sin(p5num(List__[0])) };
CORE.sqrt  = function(List__) { return Math.sqrt(p5num(List__[0])) };
CORE.atan2 = function(List__) { return Math.atan2(p5num(List__[0]), p5num(List__[1])) };
CORE.int   = function(List__) { 
    var v = p5num(List__[0]);
    return v > 0 ? Math.floor(v) : Math.ceil(v)
};

CORE.rand = function(List__) {
    var v = p5num(List__[0]) || 1;
    return Math.random() * v;
};

CORE.lc      = function(List__) { return p5str(List__[0]).toLowerCase() };
CORE.uc      = function(List__) { return p5str(List__[0]).toUpperCase() };

CORE.lcfirst = function(List__) {
    var s = p5str(List__[0]);
    var c = s.length > 0 ? s.slice(0,1).toLowerCase() : "";
    s = s.length > 1 ? s.substr(1) : "";
    return c + s
};

CORE.ucfirst = function(List__) {
    var s = p5str(List__[0]);
    var c = s.length > 0 ? s.slice(0,1).toUpperCase() : "";
    s = s.length > 1 ? s.substr(1) : "";
    return c + s
};

CORE.quotemeta = function(List__) {
    var s = p5str(List__[0]);
    var out = [];
    for(var i = 0; i < s.length; i++) {
        if (s.substr(i, 1).match(/[^0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz]/)) {
            out.push(String.fromCharCode(92));
        }
        out.push(s.substr(i, 1));
    }
    return out.join("");       
};

CORE.substr = function(List__) {

    // TODO - lvalue substr()

    var s           = p5str(List__[0]);
    var offset      = p5num(List__[1]);
    var length      = List__.length > 2 ? p5num(List__[2]) : s.length;
    var replacement = List__[3];

    if (length < 0) {
        length = s.length - offset + length;
    }
    return s.substr(offset, length);
};

CORE.defined = function(List__) {
    return (List__[0] instanceof p5Scalar) ? ( List__[0].FETCH() != null ) : ( List__[0] != null )
};

CORE.values = function(List__, p5want) {
    var o = List__[0]._hash_;
    delete o["_each_"];
    if (p5want) {
        if (o == null) {
            return new p5Array([]);
        };
        if (typeof o.values === "function") {
            return o.values();
        }
        var out = [];
        for (var i in o) {
            out.push(o[i]);
        }
        return new p5Array(out);
    }
    return CORE.keys(List__, p5want);
};

CORE.keys = function(List__, p5want) {
    var o = List__[0]._hash_;
    delete o["_each_"];
    if (p5want) {
        if (o == null) {
            return new p5Array([]);
        }
        if (typeof o.keys === "function") {
            return o.keys();
        }
        var out = [];
        for (var i in o) {
            out.push(i);
        }
        return new p5Array(out);
    }
    else {
        if (o == null) {
            return 0;
        }
        if (typeof o.keys === "function") {
            return CORE.scalar([o.keys()]);
        }
        var out = 0;
        for (var i in o) {
            out++;
        }
        return out;
    }
};

CORE.each = function(List__, p5want) {
    var o = List__[0];
    if (o.hasOwnProperty("_each_")) {
        return o._each_(p5want)
    }
    var keys = CORE.keys([o], 1);
    var i = 0;
    o._each_ = function () {
        if (i < keys.length) {
            i++;
            return p5want ? [keys[i-1], o[keys[i-1]]] : keys[i-1];
        }
        i = 0;
        return p5want ? [] : null;
    };
    return o._each_(p5want);
};

CORE.reverse = function(List__) {
    var o = List__[0];
    if (o instanceof p5Scalar) {
        o = o.FETCH();
    }
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

CORE.splice = function(List__, p5want) {
    var array  = List__.shift()._array_;
    // CORE.say([ array ]);
    var offset = p5num(List__.shift());
    var limit  = List__.length ? p5num(List__.shift()) : (array.length + 1);

    if (limit < 0) {
        limit = array.length + limit - 1;
    }

    var list = [offset, limit];
    for(var i = 0; i < List__.length; i++) {
        list = p5list_to_a( list, List__[i]);
    }

    out = array.splice.apply(array, list);
    // CORE.say([ CORE.join([":",array]), " ofs=", offset, " lim=", limit, " list=", list, " out=", CORE.join([":",out])  ]);
    return p5want ? out : out.pop();
};

CORE.pop = function(List__) {
    return List__[0].POP();
};

CORE.shift = function(List__) {
    return List__[0].SHIFT();
};

CORE.push = function(List__) {
    return List__[0].PUSH(List__[1]);
};

CORE.unshift = function(List__) {
    return List__[0].UNSHIFT(List__[1]);
};

CORE.join = function(List__) {
    var s = List__[0];
    var o = List__[1]._array_;
    var a = [];
    for (i = 0; i < o.length; i++) {
        a.push( p5str(o[i]) );
    }
    return a.join(s);
};

CORE.index = function(List__) {
    try {
        return p5str(List__[0]).indexOf(p5str(List__[1]), p5num(List__[2]));
    }
    catch(err) {
        return -1;
    }
};
CORE.rindex = function(List__) {
    var o = p5str(List__[0]);
    var s = List__[1];
    try {
        if (List__.length > 2) {
            var i = p5num(List__[2]);
            if (i < 0) {
                if (s.length == 0) {
                    return 0;
                }
                return -1;
            }
            return o.lastIndexOf(s, i);
        }
        return o.lastIndexOf(s);
    }
    catch(err) {
        return -1;
    }
};

CORE.length = function(List__) {
    var o = p5str(List__[0]);
    return o.length;
};

CORE.pack    = function(List__) { CORE.warn([ "CORE::pack not implemented" ]) };
CORE.unpack  = function(List__) { CORE.warn([ "CORE::unpack not implemented" ]) };

CORE.ref = function(List__) {
    var o = List__[0];
    if (o instanceof p5Scalar) {
        o = o.FETCH();
    }
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
    var s       = p5str(List__[1]);
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
        return s.split(pattern);
    }
    CORE.die(["not implemented"]);
};

CORE.prototype = function(List__, data) {
    var name = List__[0];
    // TODO - fully qualify "name" using information from "data"
    // XXX - lookup in CORE::GLOBAL?
    p5pkg["Perlito5"].v_PROTO._hash_[name] || p5pkg["Perlito5"].v_CORE_PROTO._hash_[name]
};

EOT
} # end of emit_javascript3()

1;

