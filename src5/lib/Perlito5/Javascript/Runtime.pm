use v5;

package Perlito5::Javascript::Runtime;

sub emit_javascript {

    return '//
// lib/Perlito5/Javascript/Runtime.js
//
// Runtime for "Perlito" Perl5-in-Javascript
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

if (typeof NAMESPACE !== "object") {
    NAMESPACE = {};
    LOCAL = [];

    var universal = function () {};
    NAMESPACE.UNIVERSAL = new universal();
    NAMESPACE.UNIVERSAL._ref_ = "UNIVERSAL";
    NAMESPACE.UNIVERSAL.isa = function (List__) {
        // TODO - use @ISA
        return List__[0]._class_._ref_ == List__[1]
    };
    NAMESPACE.UNIVERSAL.can = function (List__) {
        var o = List__[0];
        var s = List__[1];
        if ( s.indexOf("::") == -1 ) {
            // TODO - use _method_lookup_
            return o._class_[s]
        }
        var c = s.split("::");
        s = c.pop(); 
        // TODO - use _method_lookup_
        return _method_lookup_(s, c.join("::"), {});
    };
    NAMESPACE.UNIVERSAL.DOES = NAMESPACE.UNIVERSAL.can;

    var core = function () {};
    NAMESPACE["CORE"] = new core();
    NAMESPACE["CORE"]._ref_ = "CORE";

    var core_global = function () {};
    core_global.prototype = NAMESPACE.CORE;
    NAMESPACE["CORE::GLOBAL"] = new core_global();
    NAMESPACE["CORE::GLOBAL"]._ref_ = "CORE::GLOBAL";

    p5_error = function (v) {
        this.v = v;
        this.toString = function(){ return this.v };
    };
    p5_error.prototype = Error.prototype;
}

function make_package(pkg_name) {
    if (!NAMESPACE.hasOwnProperty(pkg_name)) {
        var tmp = function () {};
        tmp.prototype = NAMESPACE["CORE::GLOBAL"];
        NAMESPACE[pkg_name] = new tmp();
        NAMESPACE[pkg_name]._ref_ = pkg_name;
        NAMESPACE[pkg_name]._class_ = NAMESPACE[pkg_name];  // XXX memory leak

        // TODO - add the other package global variables
        NAMESPACE[pkg_name]["List_ISA"] = [];
        NAMESPACE[pkg_name]["v_a"] = null;
        NAMESPACE[pkg_name]["v_b"] = null;
        NAMESPACE[pkg_name]["v__"] = null;
        NAMESPACE[pkg_name]["v_^O"] = isNode ? "node.js" : "javascript";
    }
    return NAMESPACE[pkg_name];
}

function _method_lookup_(method, class_name, seen) {
    // default mro
    c = NAMESPACE[class_name];
    if ( c.hasOwnProperty(method) ) {
        return c[method]
    }
    var isa = c.List_ISA;
    for (var i = 0; i < isa.length; i++) {
        if (!seen[isa[i]]) {
            var m = _method_lookup_(method, isa[i]);
            if (m) {
                return m 
            }
            seen[isa[i]]++;
        }
    }
    // TODO - AUTOLOAD
}

function _call_(invocant, method, list) {
    list.unshift(invocant);
    if ( invocant._class_.hasOwnProperty(method) ) {
        return invocant._class_[method](list) 
    }
    var m = _method_lookup_(method, invocant._class_._ref_, {});
    if (m) {
        return m(list)
    }
    if ( NAMESPACE.UNIVERSAL.hasOwnProperty(method) ) {
        return NAMESPACE.UNIVERSAL[method](list) 
    }

    // method can have an optional namespace
    var package = method.split(/::/);
    if (package.length > 1) {
        var name = package.pop();
        package = package.join("::");
        m = _method_lookup_(name, package, {});
        // CORE.say([ name, " ", package ]);
        if (m) {
            return m(list)
        }
        NAMESPACE.CORE.die(["method not found: ", name, " in class ", package]);
    }

    // TODO - cache the methods that were already looked up
    NAMESPACE.CORE.die(["method not found: ", method, " in class ", invocant._ref_]);
}

make_package("main");
NAMESPACE["main"]["v_@"] = [];      // $@
NAMESPACE["main"]["List_#"] = [];   // @#

make_package("Perlito5");
make_package("Perlito5::IO");
make_package("Perlito5::Runtime");
make_package("Perlito5::Grammar");

function make_sub(pkg_name, sub_name, func) {
    NAMESPACE[pkg_name][sub_name] = func;
}

function set_local(namespace, name, sigil) {
    var v = name;
    if (sigil == "$") {
        v = "v_"+name;
    }
    LOCAL.push([namespace, v, namespace[v]]);
}

function cleanup_local(idx, value) {
    while (LOCAL.length > idx) {
        l = LOCAL.pop();
        l[0][l[1]] = l[2];
    }
    return value;
}

if (isNode) {
    List_ARGV = process.argv.splice(2);
} else if (typeof arguments === "object") {
    List_ARGV = arguments;
}

function HashRef(o) {
    this._hash_ = o;
    this._ref_ = "HASH";
    this.bool = function() { return 1 };
}

function ArrayRef(o) {
    this._array_ = o;
    this._ref_ = "ARRAY";
    this.bool = function() { return 1 };
}

function ScalarRef(o) {
    this._scalar_ = o;
    this._ref_ = "SCALAR";
    this.bool = function() { return 1 };
}

if (isNode) {
    var fs = require("fs");
    make_sub("Perlito5::IO", "slurp", function(List__) {
        return fs.readFileSync(List__[0],"utf8");
    });
} else {
    make_sub("Perlito5::IO", "slurp", function(List__) {
        var filename = List__[0];
        if (typeof readFile == "function") {
            return readFile(filename);
        }
        if (typeof read == "function") {
            // v8
            return read(filename);
        }
        NAMESPACE.CORE.die(["Perlito5::IO::slurp() not implemented"]);
    });
}

p5context = function(List__, p5want) {
    if (p5want) {
        return interpolate_array.apply(null, List__);
    }
    // scalar: return the last value
    var o = List__;
    while (o instanceof Array) {
        o =   o.length
            ? o[o.length-1]
            : null;
    }
    return o;
}

interpolate_array = function() {
    var res = [];
    for (i = 0; i < arguments.length; i++) {
        var o = arguments[i];
        if  (  o == null
            || o._class_    // perl5 blessed reference
            || o._ref_      // perl5 un-blessed reference
            )
        {
            res.push(o);
        }
        else if (o instanceof Array) {
            // perl5 array
            for (j = 0; j < o.length; j++) {
                res.push(o[j]);
            }
        }
        else if (typeof o === "object") {
            // perl5 hash
            for(var j in o) {
                if (o.hasOwnProperty(j)) {
                    res.push(j);
                    res.push(o[j]);
                }
            }
        }
        else {
            // non-ref
            res.push(o);
        }
    }
    return res;
};

array_to_hash = function(a) {
    var res = {};
    for (i = 0; i < a.length; i+=2) {
        res[p5str(a[i])] = a[i+1];
    }
    return res;
};

p5str = function(o) {
    if (o == null) {
        return "";
    }
    if (typeof o === "object" && (o instanceof Array)) {
        return "" + o.length;
    }
    if (typeof o.string === "function") {
        return o.string();
    }
    if (typeof o == "number" && Math.abs(o) < 0.0001 && o != 0) {
        return o.toExponential().replace(/e-(\d)$/,"e-0$1");
    }
    if (typeof o !== "string") {
        return "" + o;
    }
    return o;
};

num = function(o) {
    if (o == null) {
        return 0;
    }
    if (typeof o === "object" && (o instanceof Array)) {
        return o.length;
    }
    if (typeof o.num === "function") {
        return o.num();
    }
    if (typeof o !== "number") {
        return parseFloat(p5str(o));
    }
    return o;
};

bool = function(o) {
    if (o == null) {
        return o;
    }
    if (typeof o === "boolean") {
        return o;
    }
    if (typeof o === "number") {
        return o;
    }
    if (typeof o === "string") {
        return o != "" && o != "0";
    }
    if (typeof o.bool === "function") {
        return o.bool();
    }
    if (typeof o.length === "number") {
        return o.length;
    }
    if (o instanceof Error) {
        return true;
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

p5cmp = function(a, b) {
    return a > b ? 1 : a < b ? -1 : 0 
};

str_replicate = function(o, n) {
    n = num(n);
    return n ? Array(n + 1).join(o) : "";
};

p5for = function(namespace, func, args) {
    var v_old = namespace["v__"];
    for(var i = 0; i < args.length; i++) {
        namespace["v__"] = args[i];
        func()
    }
    namespace["v__"] = v_old;
};

p5for_lex = function(func, args) {
    for(var i = 0; i < args.length; i++) {
        func(args[i])
    }
};

p5map = function(namespace, func, args) {
    var v_old = namespace["v__"];
    var out = [];
    for(var i = 0; i < args.length; i++) {
        namespace["v__"] = args[i];
        var o = interpolate_array(func(1));
        for(var j = 0; j < o.length; j++) {
            out.push(o[j]);
        }
    }
    namespace["v__"] = v_old;
    return out;
};

p5grep = function(namespace, func, args) {
    var v_old = namespace["v__"];
    var out = [];
    for(var i = 0; i < args.length; i++) {
        namespace["v__"] = args[i];
        if (bool(func(0))) {
            out.push(args[i])
        }
    }
    namespace["v__"] = v_old;
    return out;
};

p5sort = function(namespace, func, args) {
    var a_old = namespace["v_a"];
    var b_old = namespace["v_b"];
    var out = 
        func == null
        ? args.sort()
        : args.sort(
            function(a, b) {
                namespace["v_a"] = a;
                namespace["v_b"] = b;
                return func(0);
            }
        );
    namespace["v_a"] = a_old;
    namespace["v_b"] = b_old;
    return out;
};

function perl5_to_js( source, namespace, var_env_js ) {
    // say( "source: [" + source + "]" );

    var strict_old = NAMESPACE["Perlito5"].v_STRICT;
    var var_env_js_old = NAMESPACE["Perlito5"].v_VAR;
    NAMESPACE["Perlito5"].v_VAR = var_env_js;

    var namespace_old = NAMESPACE["Perlito5"].v_PKG_NAME;
    NAMESPACE["Perlito5"].v_PKG_NAME = namespace;

    match = _call_(NAMESPACE["Perlito5::Grammar"], "exp_stmts", [source, 0]);

    if ( !match || match._hash_.to != source.length ) {
        CORE.die(["Syntax error in eval near pos ", match._hash_.to]);
    }

    ast = NAMESPACE.CORE.bless([
        new HashRef({
            block:  NAMESPACE.CORE.bless([
                        new HashRef({
                            stmts:   _call_(match, "flat", []),
                        }),
                        NAMESPACE["Perlito5::AST::Lit::Block"]
                    ]),
        }),
        NAMESPACE["Perlito5::AST::Do"]
    ]);

    // CORE.say( "ast: [" + perl(ast) + "]" );
    js_code = _call_(ast, "emit_javascript", []);
    // CORE.say( "js-source: [" + js_code + "]" );

    NAMESPACE["Perlito5"].v_PKG_NAME = namespace_old;
    NAMESPACE["Perlito5"].v_VAR      = var_env_js_old;
    NAMESPACE["Perlito5"].v_STRICT = strict_old;
    return js_code;
}
';
} # end of emit_javascript()

1;


