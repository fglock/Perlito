use v5;

package Perlito5::JavaScript2::CORE;

sub emit_javascript2 {

    return <<'EOT'
//
//
// lib/Perlito5/JavaScript2/CORE.js
//
// CORE functions for "Perlito" Perl5-in-JavaScript2
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

EOT

# sleep()

    . <<'EOT'
if (isNode) {
    try {
        var sleep = require("sleep");
        CORE.sleep = function(List__) {
            var n = p5num(List__[0]) || 1;
            sleep.usleep(n * 1000000);  // sleep for n seconds (1 second is 1000000 microseconds)
            return n;
        }
    }
    catch (err) {
        CORE.sleep = function(List__) {
            CORE.die("sleep() function failed. Maybe you need 'npm install sleep'?\n" + err);
        }
    }
}
if (!CORE.sleep) {
    CORE.sleep = function(List__) {
        CORE.die("sleep() not supported for this platform");
    }
}

EOT

# crypt()

    . <<'EOT'
if (isNode) {
    try {
        var crypt = require("crypt3");
        CORE.crypt = function(List__) {
            var text = p5str(List__[0]);
            var salt = p5str(List__[1]);
            while(salt.length < 2) {
                salt += "A";
            }
            return crypt(text, salt);
        }
    }
    catch (err) {
        CORE.crypt = function(List__) {
            CORE.die("crypt() function failed. Maybe you need 'npm install crypt3'?\n" + err);
        }
    }
}
if (!CORE.crypt) {
    CORE.crypt = function(List__) {
        CORE.die("crypt() not supported for this platform");
    }
}

EOT

# time()

    . <<'EOT'
CORE.time = function(List__) {
    return CORE.int([Date.now() / 1000]);
}
EOT

#
# Note: gmtime / localtime test:
#
# $ node perlito5.js -I src5/lib -e ' print gmtime . " @{[ gmtime ]}\n" . localtime . " @{[ localtime ]}\n" ' ; perl -e ' print gmtime . " @{[ gmtime ]}\n" . localtime . " @{[ localtime ]}\n" '
#
# TODO - isdst is not implemented
# See:
# http://stackoverflow.com/questions/11887934/check-if-daylight-saving-time-is-in-effect-and-if-it-is-for-how-many-hours
#

    . <<'EOT'
var _fmt_date = function(date) {
    return ['Sun','Mon','Tue','Wed','Thu','Fri','Sat'][date.getDay()] + ' ' +
        ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'][date.getMonth()] + ' ' +
        date.getDate() + ' ' + 
        CORE.sprintf([ "%02d:%02d:%02d ", date.getHours(), date.getMinutes(), date.getSeconds() ]) +
        date.getFullYear();
}
var _list_date = function(date) {
    var year_start = new Date(date);
    year_start.setMonth(0, 1);
    var year_day = Math.round((date-year_start)/8.64e7);

    var isdst = 0;  // not implemented

    return [date.getSeconds(),date.getMinutes(),date.getHours(),date.getDate(),
        date.getMonth(),date.getFullYear()-1900,date.getDay(),
        year_day,
        isdst
    ];
}
CORE.localtime = function(List__, want) {
    var n = List__.length ? p5num(List__[0]) : CORE.time() ;
    var date = new Date(n*1000);
    if (!want) {
        return _fmt_date(date);
    }
    return _list_date(date);
}
CORE.gmtime = function(List__, want) {
    var n = List__.length ? p5num(List__[0]) : CORE.time() ;
    var ofs = new Date().getTimezoneOffset() * 60;
    var date = new Date((n + ofs)*1000);
    if (!want) {
        return _fmt_date(date);
    }
    return _list_date(date);
}

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
    var caller = p5pkg["Perlito5"].List_CALLER;
    if ( caller && caller[0] ) {
        // TODO
        if (want) {
            return caller[0]._array_;
        }
        return caller[0]._array_[0];
    }
    return p5context([null, null, null], want);
};

CORE.vec = function(List__) {
    var sb = p5str(List__[0]);
    var offset = p5num(List__[1]);
    var bits = p5num(List__[2]);
    if (offset < 0) {
        return CORE.die(["Negative offset to vec in lvalue context: " + offset]);
    }
    if (bits == 1) {
        var byteOfs = Math.floor(offset / 8);
        var bitOfs  = offset - 8 * byteOfs;
        var mask = 1;
        if (byteOfs < sb.length) {
            return (sb.charCodeAt(byteOfs) >> bitOfs) & mask;
        }
        else {
            return 0;
        }
    }
    if (bits == 2) {
       var byteOfs = Math.floor(offset / 4);
       var bitOfs  = 2 * (offset - 4 * byteOfs);
       var mask = 3;
       if (byteOfs < sb.length) {
           return (sb.charCodeAt(byteOfs) >> bitOfs) & mask;
       }
       else {
           return 0;
       }
    } 
    if (bits == 4) {
       var byteOfs = Math.floor(offset / 2);
       var bitOfs  = 4 * (offset - 2 * byteOfs);
       var mask = 15;
       if (byteOfs < sb.length) {
           return (sb.charCodeAt(byteOfs) >> bitOfs) & mask;
       }
       else {
           return 0;
       }
    } 
    if (bits == 8) {
        return sb.charCodeAt(offset) & 255;
    }
    if (bits == 16) {
        return (sb.charCodeAt(offset) & 255) + 256 * (sb.charCodeAt(offset+1) & 255);
    }
    if (bits == 32) {
        return (sb.charCodeAt(offset) & 255) +
            256 * (sb.charCodeAt(offset+1) & 255) +
            256 * 256 * (sb.charCodeAt(offset+2) & 255) +
            256 * 256 * 256 * (sb.charCodeAt(offset+3) & 255);
    }
    return CORE.die(["Illegal number of bits in vec: " + bits]);
};

CORE.chr = function(List__) {
    var v = p5num(List__[0]);
    return String.fromCharCode(v >= 0 ? v : 65533);
};

CORE.ord = function(List__) {
    return p5str(List__[0]).charCodeAt(0);
};

CORE.hex = function(List__) {
    var v = List__[0];

    for(var i = 0; i < v.length; i++) {
        if (v.charCodeAt(i) > 255) {
            CORE.die(["Wide character in hex"]);
        }
    }

    var b1 = v.substr(0,1);
    var b2 = v.substr(0,2);
    if (b1 == "x" || b1 == "X" || b2 == "0x" || b2 == "0X") {
        return CORE.oct(List__);
    }
    v = "0x" + v;
    return CORE.oct([v]);
};

CORE.oct = function(List__) {
    var v = List__[0];
    v = v.trim();

    for(var i = 0; i < v.length; i++) {
        if (v.charCodeAt(i) > 255) {
            CORE.die(["Wide character in oct"]);
        }
    }

    var b = v.substr(0,1);
    if (b == "b" || b == "B" || b == "x" || b == "X") {
        v = "0" + v;
    }
    b = v.substr(0,2);

    for(var i = 2; i < v.length; i++) {
        if (v.substr(i,2) == "__") {
            v = v.substr(0, i);
        }
    }

    var re = new RegExp('_', 'g');
    v = v.replace(re, "");
    var result;

    if (b == "0b" || b == "0B") {
        for(var i = 2; i < v.length; i++) {
            var c = v.substr(i,1);
            if (c >= "0" && c <= "1") {}
            else {
                v = v.substr(0, i);
            }
        }
        if (v.length == 2) { return 0 }
        result = parseInt(v.substr(2), 2);
    }
    else if (b == "0x" || b == "0X") {
        for(var i = 2; i < v.length; i++) {
            var c = v.substr(i,1);
            if (c >= "0" && c <= "9" || c >= "A" && c <= "F" || c >= "a" && c <= "f") {}
            else {
                v = v.substr(0, i);
            }
        }
        if (v.length == 2) { return 0 }
        result = parseInt(v.substr(2), 16);
    }
    else {
        result = parseInt(v, 8);
    }
    return isNaN(result) ? 0 : result;
};

CORE.abs   = function(List__) { return Math.abs(List__[0]) };
CORE.exp   = function(List__) { return Math.exp(List__[0]) };
CORE.log   = function(List__) { return Math.log(List__[0]) };
CORE.cos   = function(List__) { return Math.cos(List__[0]) };
CORE.sin   = function(List__) { return Math.sin(List__[0]) };
CORE.sqrt  = function(List__) { return Math.sqrt(List__[0]) };
CORE.atan2 = function(List__) { return Math.atan2(List__[0], List__[1]) };
CORE.int   = function(List__) { return List__[0] > 0 ? Math.floor(List__[0]) : Math.ceil(List__[0]) };

var p5rand = function(v) { return Math.random() * v };
CORE.srand = function(List__) {
    if (List__.length > 0) {
        var v = p5num(List__[0]) || 1;
        p5rand = function() {
            v = Math.sin(v) * 10000;
            return v - Math.floor(v);
        };
        return List__[0];
    }
    return CORE.int(CORE.rand(100000));
};
CORE.rand = function(List__) {
    var v = p5num(List__[0]) || 1;
    return p5rand(v);
};

CORE.lc      = function(List__) { return p5str(List__[0]).toLowerCase() };
CORE.uc      = function(List__) { return p5str(List__[0]).toUpperCase() };
CORE.fc      = function(List__) { return p5str(List__[0]).toUpperCase() };

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

CORE.reverse = function(List__, p5want) {
    var o = List__;
    if (p5want) {
        if (o == null) {
            return [];
        }
        return List__.reverse();
    }
    o = p5str(o);
    return o.split("").reverse().join("")
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
        list = p5list_to_a([ list, List__[i] ]);
    }

    var out = array.splice.apply(array, list);
    // CORE.say([ CORE.join([":",array]), " ofs=", offset, " lim=", limit, " list=", list, " out=", CORE.join([":",out])  ]);
    return p5want ? out : out.pop();
};

CORE.join = function(List__) {
    var s = List__.shift();
    var o = [];
    for (var i = 0; i < List__.length; i++) {
        o.push(p5str(List__[i]));
    }
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

CORE.pack    = function(List__) {
    // pack "W*", 90
    var pattern = p5str(List__.shift());
    if (pattern == "W*") {
        return List.map(String.fromCharCode).join('');
    }
    if (pattern == "d") {
        return "";  // TODO
    }
    CORE.warn([ "CORE::pack " + pattern + " not implemented" ]);
};
CORE.unpack  = function(List__) {
    var pattern = p5str(List__[0]);
    var s = p5str(List__[1]);
    if (pattern == "W*") {
        return s.split('').map( x => x.charCodeAt(0) );
    }
    CORE.warn([ "CORE::unpack " + pattern + " not implemented" ])
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

CORE.split = function(List__, want) {
    var pattern = List__[0];
    var s       = p5str(List__[1]);
    var limit   = p5num(List__[2]);
    if (!want) {
        // scalar context
        return p5num(CORE.split(List__, 1));
    }
    if (limit == 0) {
        // strip trailing empty strings and undef
        var res = CORE.split([pattern, s, -1], 1);
        while (res.length && (res[res.length - 1] == '' || typeof res[res.length - 1] == "undefined") ) {
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
    var count = 0;
    while (1) {
        if (limit > 0 && limit <= (count + 1)) {
            res.push(s.substr(pos));
            return res;
        }
        var m = pattern.exec(s);
        if (m === null) {
            // no match
            res.push(s.substr(pos));
            return res;
        }
        if (m[0].length == 0 && m.index == pos) {
            // pointer didn't move
            pattern.lastIndex = pattern.lastIndex + 1;
        }
        else {
            var part = s.substr(pos, m.index - pos);
            res.push(part);
            count++;
            pos = m.index + m[0].length;
            pattern.lastIndex = pos;
        }
        for (var i = 1; i < m.length ; i++) {
            res.push(m[i]);     // captured substrings; don't increment count
        }
    }
};


EOT
} # end of emit_javascript2()

1;

