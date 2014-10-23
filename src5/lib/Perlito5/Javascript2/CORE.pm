use v5;

package Perlito5::Javascript2::CORE;

sub emit_javascript2 {

    return <<'EOT'
//
//
// lib/Perlito5/Javascript2/CORE.js
//
// CORE functions for "Perlito" Perl5-in-Javascript2
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

CORE.bless = function(List__) {
    var o        = List__[0];
    var pkg_name = List__[1];
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

CORE.caller = function(List__, want) {
    return p5pkg["Perlito5"].v_CALLER._array_ ?
           p5pkg["Perlito5"].v_CALLER._array_[0]._array_ :
           p5context([], want);
};

CORE.chr = function(List__) {
    var v = p5num(List__[0]);
    return String.fromCharCode(v >= 0 ? v : 65533);
};

CORE.ord = function(List__) {
    return p5str(List__[0]).charCodeAt(0);
};

CORE.oct = function(List__) {
    var v = List__[0];
    var b = v.substr(0,2);
    v = v.replace("_", "");
    if (b == "0b" || b == "0B") { return parseInt(v.substr(2), 2)  }
    if (b == "0x" || b == "0X") { return parseInt(v.substr(2), 16) }
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
    var expr        = List__[0];
    var offset      = List__[1];
    var length      = List__[2];
    var replacement = List__[3];
    if (length < 0) {
        var s = p5str(expr);
        length = s.length - offset + length;
    } 
    return p5str(expr).substr(offset, length);
};

CORE.values = function(List__, p5want) {
    var o = List__[0];
    delete o["_each_"];
    if (p5want) {
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
    }
    return CORE.keys(List__, p5want);
};

CORE.keys = function(List__, p5want) {
    var o = List__[0];
    delete o["_each_"];
    if (p5want) {
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
    }
    else {
        if (o == null) {
            return 0;
        }
        if (typeof o.keys === "function") {
            return p5num(o.keys());
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
    var array  = List__.shift();
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

CORE.join = function(List__) {
    var s = List__[0];
    var o = List__[1];
    return o.join(s);
};

CORE.index = function(List__) {
    var o = List__[0];
    var s = List__[1];
    try {
        return o.indexOf(s, p5num(List__[2]));
    }
    catch(err) {
        return -1;
    }
};
CORE.rindex = function(List__) {
    var o = List__[0];
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
    return p5str(List__[0]).length;
};

CORE.pack    = function(List__) { CORE.warn([ "CORE::pack not implemented" ]) };
CORE.unpack  = function(List__) { CORE.warn([ "CORE::unpack not implemented" ]) };

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

CORE.split = function(List__, want) {
    var pattern = List__[0];
    var s       = p5str(List__[1]);
    var limit   = p5num(List__[2]);
    if (!want) {
        // scalar context
        return p5num(CORE.split(List__, 1));
    }
    if (limit == 0) {
        // strip trailing empty strings
        var res = CORE.split([pattern, s, -1], 1);
        while (res.length && res[res.length - 1] == '') {
            res.pop()
        }
        return res;
    }
    if (s == '') {
        return []
    }
    // make sure pattern is a RegExp
    if (typeof pattern === "object" && (pattern instanceof RegExp)) {
        pattern = pattern.source;
    }
    else {
        pattern = p5str(pattern);
        if (pattern == " ") {
            // single space string is special
            pattern = "(?: |\t|\n)+";
            s = s.replace(/^(?: |\t|\n)+/, "");
        }
    }
    // add "g", "m" modifiers
    var flags = "g";
    if (pattern.substr(0, 1) == "^" || pattern.substr(-1,1) == "$") {
        flags = flags + "m";
    }
    pattern = new RegExp(pattern, flags);
    var res = [];
    var pos = 0;
    while (1) {
        // CORE.say(["limit ",limit,"res.length ", res.length]);
        if (limit > 0 && limit <= (res.length + 1)) {
            res.push(s.substr(pos));
            // CORE.say([ p5pkg["Perlito5::Dumper"].Dumper([ new p5ArrayRef(res) ]) ]);
            return res;
        }
        var m = pattern.exec(s);
        if (m === null) {
            // no match
            res.push(s.substr(pos));
            // CORE.say([ p5pkg["Perlito5::Dumper"].Dumper([ new p5ArrayRef(res) ]) ]);
            return res;
        }
        // CORE.say([ "** index ", m.index, " matched /", m[0], "/",m[0].length," captured /", m[1], "/ next ", pattern.lastIndex ]);
        if (m[0].length == 0 && m.index == pos) {
            // pointer didn't move
            pattern.lastIndex = pattern.lastIndex + 1;
        }
        else {
            var part = s.substr(pos, m.index - pos);
            res.push(part)
            pos = m.index + m[0].length;
            pattern.lastIndex = pos;
        }
        for (var i = 1; i < m.length ; i++) {
            if (typeof m[i] != "undefined") {
                // CORE.say([ p5pkg["Perlito5::Dumper"].Dumper([ m[i] ]) ]);
                res.push(m[i]);     // captured substrings
            }   
            else {
                res.push("");     // captured substrings
            }   
        }
    }
};


EOT
} # end of emit_javascript2()

1;

