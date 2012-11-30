use v5;

package Perlito5::Javascript2::Runtime;

sub emit_javascript2 {

    return <<'EOT';
//
// lib/Perlito5/Javascript2/Runtime.js
//
// Runtime for "Perlito" Perl5-in-Javascript2
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

if (typeof p5pkg !== "object") {
    p5pkg = {};
    p5LOCAL = [];

    var universal = function () {};
    p5pkg.UNIVERSAL = new universal();
    p5pkg.UNIVERSAL._ref_ = "UNIVERSAL";
    p5pkg.UNIVERSAL.isa = function (List__) {
        // TODO - use @ISA
        return List__[0]._class_._ref_ == List__[1]
    };
    p5pkg.UNIVERSAL.can = function (List__) {
        var o = List__[0];
        var s = List__[1];
        if ( s.indexOf("::") == -1 ) {
            return p5method_lookup(s, o._class_._ref__, {})
        }
        var c = s.split("::");
        s = c.pop(); 
        return p5method_lookup(s, c.join("::"), {});
    };
    p5pkg.UNIVERSAL.DOES = p5pkg.UNIVERSAL.can;

    var core = function () {};
    p5pkg["CORE"] = new core();
    p5pkg["CORE"]._ref_ = "CORE";

    var core_global = function () {};
    core_global.prototype = p5pkg.CORE;
    p5pkg["CORE::GLOBAL"] = new core_global();
    p5pkg["CORE::GLOBAL"]._ref_ = "CORE::GLOBAL";

    p5_error = function (type, v) {
        this.type = type;
        this.v = v;
        this.toString = function(){
            if (this.type == 'break') {
                return 'Can\'t "break" outside a given block'
            }
            if (this.type == 'next' || this.type == 'last' || this.type == 'redo') {
                if (this.v == "") { return 'Can\'t "' + this.type + '" outside a loop block' }
                return 'Label not found for "' + this.type + ' ' + this.v + '"';
            }
            return this.v;
        };
    };
    p5_error.prototype = Error.prototype;
}

function p5make_package(pkg_name) {
    if (!p5pkg.hasOwnProperty(pkg_name)) {
        var tmp = function () {};
        tmp.prototype = p5pkg["CORE::GLOBAL"];
        p5pkg[pkg_name] = new tmp();
        p5pkg[pkg_name]._ref_ = pkg_name;
        p5pkg[pkg_name]._class_ = p5pkg[pkg_name];  // XXX memory leak
        p5pkg[pkg_name]._is_package_ = 1;

        // TODO - add the other package global variables
        p5pkg[pkg_name]["List_ISA"] = [];
        p5pkg[pkg_name]["v_a"] = null;
        p5pkg[pkg_name]["v_b"] = null;
        p5pkg[pkg_name]["v__"] = null;
        p5pkg[pkg_name]["v_AUTOLOAD"] = null;
    }
    return p5pkg[pkg_name];
}

function p5code_lookup_by_name(package_name, sub_name) {
    // sub_name can have an optional namespace
    var parts = sub_name.split(/::/);
    if (parts.length > 1) {
        sub_name = parts.pop();
        package_name = parts.join("::");
    }
    if (p5pkg.hasOwnProperty(package_name)) {
        var c = p5pkg[package_name];
        if ( c.hasOwnProperty(sub_name) ) {
            return c[sub_name]
        }
    }
    return null;
}

function p5get_class_for_method(method, class_name, seen) {
    // default mro
    // TODO - cache the methods that were already looked up
    if ( p5pkg[class_name].hasOwnProperty(method) ) {
        return class_name
    }
    var isa = p5pkg[class_name].List_ISA;
    for (var i = 0; i < isa.length; i++) {
        if (!seen[isa[i]]) {
            var m = p5get_class_for_method(method, isa[i], seen);
            if (m) {
                return m 
            }
            seen[isa[i]]++;
        }
    }
}

function p5method_lookup(method, class_name, seen) {
    var c = p5get_class_for_method(method, class_name, seen);
    if (c) {
        return p5pkg[c][method]
    }
    if ( p5pkg.UNIVERSAL.hasOwnProperty(method) ) {
        return p5pkg.UNIVERSAL[method]
    }
}

function p5call(invocant, method, list, p5want) {

    if (typeof invocant === "string") {
        list.unshift(invocant);
        invocant = p5make_package(invocant);
    }
    else if ( invocant.hasOwnProperty("_is_package_") ) {
        list.unshift(invocant._ref_);   // invocant is a "package" object
    }
    else {
        list.unshift(invocant);
    }

    if ( invocant.hasOwnProperty("_class_") ) {

        if ( invocant._class_.hasOwnProperty(method) ) {
            return invocant._class_[method](list, p5want)
        }
        var m = p5method_lookup(method, invocant._class_._ref_, {});
        if (m) {
            return m(list, p5want)
        }

        // method can have an optional namespace
        var pkg_name = method.split(/::/);
        if (pkg_name.length > 1) {
            var name = pkg_name.pop();
            pkg_name = pkg_name.join("::");
            m = p5method_lookup(name, pkg_name, {});
            if (m) {
                return m(list, p5want)
            }
            p5pkg.CORE.die(["method not found: ", name, " in class ", pkg_name]);
        }

        pkg_name = p5get_class_for_method('AUTOLOAD', invocant._class_._ref_, {}) || p5get_class_for_method('AUTOLOAD', "UNIVERSAL", {});
        if (pkg_name) {
            p5pkg[pkg_name]["v_AUTOLOAD"] = invocant._class_._ref_ + "::" + method;
            return p5pkg[pkg_name]["AUTOLOAD"](list, p5want);
        }

        p5pkg.CORE.die(["method not found: ", method, " in class ", invocant._class_._ref_]);

    }

    p5pkg.CORE.die(["Can't call method ", method, " on unblessed reference"]);

}

function p5call_sub(namespace, name, list, p5want) {
    if(p5pkg[namespace].hasOwnProperty(name)) {
        return p5pkg[namespace][name](list, p5want)
    }
    if(p5pkg[namespace].hasOwnProperty("AUTOLOAD")) {
        p5pkg[namespace]["v_AUTOLOAD"] = namespace + "::" + name;
        return p5pkg[namespace]["AUTOLOAD"](list, p5want)
    }
    p5pkg.CORE.die(["Undefined subroutine &" + namespace + "::" + name]);
}

function p5scalar_deref(v) {
    if (typeof v === "string") {
        var pkg_name = v.split(/::/);
        if (pkg_name.length > 1) {
            v = pkg_name.pop();
            pkg_name = pkg_name.join("::");
        }
        else {
            pkg_name = p5pkg["Perlito5"].v_PKG_NAME;
        }
        var c = v.charCodeAt(0);
        if (c < 27) {
            v = String.fromCharCode(c + 64) + v.substr(1);
            pkg_name = 'main';
        }
        return p5pkg[pkg_name]["v_"+v];
    }
    return v._scalar_;
}

function p5scalar_deref_set(v, n) {
    if (typeof v === "string") {
        var pkg_name = v.split(/::/);
        if (pkg_name.length > 1) {
            v = pkg_name.pop();
            pkg_name = pkg_name.join("::");
        }
        else {
            pkg_name = p5pkg["Perlito5"].v_PKG_NAME;
        }
        var c = v.charCodeAt(0);
        if (c < 27) {
            v = String.fromCharCode(c + 64) + v.substr(1);
            pkg_name = 'main';
        }
        p5pkg[pkg_name]["v_"+v] = n;
        return p5pkg[pkg_name]["v_"+v];
    }
    v._scalar_ = n;
    return v._scalar_;
}

p5make_package("main");
p5make_package("Perlito5");
p5pkg["Perlito5"].v_PKG_NAME = "main";
p5pkg["main"]["v_@"] = [];      // $@
p5pkg["main"]["v_|"] = 0;       // $|
p5pkg["main"]["List_#"] = [];   // @#
p5scalar_deref_set(String.fromCharCode(15), isNode ? "node.js" : "javascript2");  // $^O
p5pkg["main"]["List_INC"] = [];
p5pkg["main"]["Hash_INC"] = {};
p5pkg["main"]["List_ARGV"] = [];
p5pkg["main"]["Hash_ENV"] = {};
if (isNode) {
    p5pkg["main"]["List_ARGV"] = process.argv.splice(2);

    p5pkg["main"]["Hash_ENV"] = {};
    for (e in process.env) p5pkg["main"]["Hash_ENV"][e] = process.env[e];

    p5pkg["main"]["v_$"]       = process.pid;
} else if (typeof arguments === "object") {
    p5pkg["main"]["List_ARGV"] = arguments;
}

p5make_package("Perlito5::IO");
p5make_package("Perlito5::Runtime");
p5make_package("Perlito5::Grammar");

function p5make_sub(pkg_name, sub_name, func) {
    p5make_package(pkg_name);
    p5pkg[pkg_name][sub_name] = func;
}

var sigils = { '@' : 'List_', '%' : 'Hash_', '$' : 'v_' };

function p5set_local(namespace, name, sigil) {
    var vname = sigils[sigil] + name;
    p5LOCAL.push([namespace, vname, namespace[vname]]);

    if (sigil == '$') {
        namespace[vname] = null;
    }
    else if (sigil == '@') {
        namespace[vname] = new p5Array([]);
    }
    else if (sigil == '%') {
        namespace[vname] = new p5Hash({});
    }
    return namespace[vname];
}

function p5cleanup_local(idx, value) {
    while (p5LOCAL.length > idx) {
        l = p5LOCAL.pop();
        l[0][l[1]] = l[2];
    }
    return value;
}

//-------- Reference

function p5HashRef(o) {
    this._hash_ = o;
    this._ref_ = "HASH";
    this.bool = function() { return 1 };
}

function p5ArrayRef(o) {
    this._array_ = o;
    this._ref_ = "ARRAY";
    this.bool = function() { return 1 };
}

function p5ScalarRef(o) {
    this._scalar_ = o;
    this._ref_ = "SCALAR";
    this.bool = function() { return 1 };
}

function p5GlobRef(o) {
    this._scalar_ = o;
    this._ref_ = "GLOB";
    this.bool = function() { return 1 };
}

//-------- Hash 

Object.defineProperty( Object.prototype, "p5hget", {
    enumerable : false,
    value : function (i) { return this[i] }
});
Object.defineProperty( Object.prototype, "p5hset", {
    enumerable : false,
    value : function (i, v) { this[i] = v; return this[i] }
});

Object.defineProperty( Object.prototype, "p5incr", {
    enumerable : false,
    value : function (i) {
        this[i] = p5incr(this[i]);
        return this[i];
    }
});
Object.defineProperty( Object.prototype, "p5postincr", {
    enumerable : false,
    value : function (i) {
        var v = this[i];
        this[i] = p5incr(this[i]);
        return v;
    }
});
Object.defineProperty( Object.prototype, "p5decr", {
    enumerable : false,
    value : function (i) {
        this[i] = p5decr(this[i]);
        return this[i];
    }
});
Object.defineProperty( Object.prototype, "p5postdecr", {
    enumerable : false,
    value : function (i) {
        var v = this[i];
        this[i] = p5decr(this[i]);
        return v;
    }
});

Object.defineProperty( Object.prototype, "p5hget_array", {
    enumerable : false,
    value : function (i) {
        if (this[i] == null) { this[i] = new p5ArrayRef([]) }
        return this[i]
    }
});
Object.defineProperty( Object.prototype, "p5hget_hash", {
    enumerable : false,
    value : function (i) {
        if (this[i] == null) { this[i] = new p5HashRef({}) }
        return this[i]
    }
});

//-------


if (isNode) {
    var fs = require("fs");
    p5make_sub("Perlito5::IO", "slurp", function(List__) {
        return fs.readFileSync(List__[0],"utf8");
    });
} else {
    p5make_sub("Perlito5::IO", "slurp", function(List__) {
        var filename = List__[0];
        if (typeof readFile == "function") {
            return readFile(filename);
        }
        if (typeof read == "function") {
            // v8
            return read(filename);
        }
        p5pkg.CORE.die(["Perlito5::IO::slurp() not implemented"]);
    });
}

p5context = function(List__, p5want) {
    if (p5want) {
        return p5list_to_a.apply(null, List__);
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

p5list_to_a = function() {
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

p5a_to_h = function(a) {
    var res = {};
    for (i = 0; i < a.length; i+=2) {
        res[p5str(a[i])] = a[i+1];
    }
    return res;
};

p5idx = function(a, i) {
    return i >= 0 ? i : a.length + i
};

p5str = function(o) {
    if (o == null) {
        return "";
    }
    if (typeof o === "object" && (o instanceof Array)) {
        return CORE.join(["", o]);
    }
    // if (typeof o.string === "function") {
    //     return o.string();
    // }
    if (typeof o == "number" && Math.abs(o) < 0.0001 && o != 0) {
        return o.toExponential().replace(/e-(\d)$/,"e-0$1");
    }
    if (typeof o === "boolean") {
        return o ? "1" : "";
    }
    if (typeof o !== "string") {
        return "" + o;
    }
    return o;
};

p5num = function(o) {
    if (o == null) {
        return 0;
    }
    if (typeof o === "object" && (o instanceof Array)) {
        return o.length;
    }
    // if (typeof o.num === "function") {
    //     return o.num();
    // }
    if (typeof o !== "number") {
        return parseFloat(p5str(o));
    }
    return o;
};

p5bool = function(o) {
    if (o) {
        if (typeof o === "boolean") {
            return o;
        }
        if (typeof o === "number") {
            return o;
        }
        if (typeof o === "string") {
            return o != "" && o != "0";
        }
        // if (typeof o.bool === "function") {
        //     return o.bool();
        // }
        if (typeof o.length === "number") {
            return o.length;
        }
        if (o instanceof Error) {
            return true;
        }
        for (var i in o) {
            return true;
        }
    }
    return false;
};

p5incr = function(o) {
    if (typeof o === "number") {
        return o + 1;
    }
    return p5str_inc(p5str(o));
};

p5decr = function(o) {
    if (typeof o === "number") {
        return o - 1;
    }
    return p5num(o) - 1;
};

p5modulo = function(o, k) {
    var m = o % k;
    if ( k < 0 && m > 0 ) {
        m = m + k;
    }
    else if ( k > 0 && m < 0 ) {
        m = m + k;
    }
    return m;
};

p5shift_left = function(o, k) {
    return k < 31 ? o << k : o * Math.pow(2, k);
};

p5and = function(a, fb) {
    if (p5bool(a)) {
        return fb();
    }
    return a;
};

p5or = function(a, fb) {
    if (p5bool(a)) {
        return a;
    }
    return fb();
};

p5defined_or = function(a, fb) {
    if (a == null) {
        return fb();
    }
    return a;
};

p5cmp = function(a, b) {
    return a > b ? 1 : a < b ? -1 : 0 
};

p5complement = function(a) {
    return a < 0 ? ~a : 4294967295 - a
    // return a < 0 ? ~a : 18446744073709551615 - a
};

p5str_replicate = function(o, n) {
    n = p5num(n);
    return n ? Array(n + 1).join(o) : "";
};

p5str_inc = function(s) {
    s = p5str(s);
    if (s.length < 2) {
        if (s.match(/[012345678ABCDEFGHIJKLMNOPQRSTUVWXYabcdefghijklmnopqrstuvwxy]/)) {
            return String.fromCharCode(s.charCodeAt(0) + 1);
        }
        if (s == "9") {
            return "10";
        }
        if (s == "Z") {
            return "AA";
        }
        if (s == "z") {
            return "aa";
        }
        return "1";
    }
    var c = p5str_inc(s.substr(s.length-1, 1));
    if (c.length == 1) {
        return s.substr(0, s.length-1) + c;
    }
    return p5str_inc(s.substr(0, s.length-1)) + c.substr(c.length-1, 1);
};

p5for = function(namespace, func, args, cont, label) {
    var _redo = false;
    var v_old = namespace["v__"];
    for(var i = 0; i < args.length; i++) {
        namespace["v__"] = args[i];
        try {
            func()
        }
        catch(err) {
            if (err instanceof p5_error && err.v == label) {
                if (err.type == 'last') { return }
                else if (err.type == 'redo') { i--; _redo = true }
                else if (err.type != 'next') { throw(err) }
            }
            else {
                throw(err)
            }
        }
        if (cont) {
            try {
                if (!_redo) { cont() }
            }
            catch(err) {
                if (err instanceof p5_error && err.v == label) {
                    if (err.type == 'last') { return }
                    else if (err.type == 'redo') { _redo = true }
                    else if (err.type != 'next') { throw(err) }
                }            
                else {
                    throw(err)
                }
            }
       }
   }
    namespace["v__"] = v_old;
};

p5for_lex = function(func, args, cont, label) {
    var _redo = false;
    for(var i = 0; i < args.length; i++) {
        try {
            func(args[i])
        }
        catch(err) {
            if (err instanceof p5_error && err.v == label) {
                if (err.type == 'last') { return }
                else if (err.type == 'redo') { i--; _redo = true }
                else if (err.type != 'next') { throw(err) }
            }            
            else {
                throw(err)
            }
        }
        if (cont) {
            try {
                if (!_redo) { cont() }
            }
            catch(err) {
                if (err instanceof p5_error && err.v == label) {
                    if (err.type == 'last') { return }
                    else if (err.type == 'redo') { _redo = true }
                    else if (err.type != 'next') { throw(err) }
                }            
                else {
                    throw(err)
                }
            }
        }
    }
};

p5while = function(func, cond, cont, label) {
    var _redo = false;
    while (_redo || p5bool(cond())) {
        _redo = false;
        try {
            func()
        }
        catch(err) {
            if (err instanceof p5_error && err.v == label) {
                if (err.type == 'last') { return }
                else if (err.type == 'redo') { _redo = true }
                else if (err.type != 'next') { throw(err) }
            }            
            else {
                throw(err)
            }
        }
        if (cont) {
            try {
                if (!_redo) { cont() }
            }
            catch(err) {
                if (err instanceof p5_error && err.v == label) {
                    if (err.type == 'last') { return }
                    else if (err.type == 'redo') { _redo = true }
                    else if (err.type != 'next') { throw(err) }
                }            
                else {
                    throw(err)
                }
            }
        }
    }
};

p5map = function(namespace, func, args) {
    var v_old = namespace["v__"];
    var out = [];
    for(var i = 0; i < args.length; i++) {
        namespace["v__"] = args[i];
        var o = p5list_to_a(func(1));
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
        if (p5bool(func(0))) {
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

perl5_to_js = function( source, namespace, var_env_js, p5want ) {
    // CORE.say(["source: [" + source + "]"]);

    var strict_old = p5pkg["Perlito5"].v_STRICT;
    var var_env_js_old = p5pkg["Perlito5"].v_VAR;
    p5pkg["Perlito5"].v_VAR = var_env_js;

    var namespace_old = p5pkg["Perlito5"].v_PKG_NAME;
    p5pkg["Perlito5"].v_PKG_NAME = namespace;

    match = p5call(p5pkg["Perlito5::Grammar"], "exp_stmts", [source, 0]);

    if ( !match || match._hash_.to != source.length ) {
        CORE.die(["Syntax error in eval near pos ", match._hash_.to]);
    }

    ast = p5pkg.CORE.bless([
        new p5HashRef({
            block:  p5pkg.CORE.bless([
                        new p5HashRef({
                            stmts:   p5pkg["Perlito5::Match"].flat([match])
                        }),
                        p5pkg["Perlito5::AST::Lit::Block"]
                    ])
        }),
        p5pkg["Perlito5::AST::Do"]
    ]);

    // CORE.say(["ast: [" + ast + "]"]);
    js_code = p5call(ast, "emit_javascript2", [0, p5want]);
    // CORE.say(["js-source: [" + js_code + "]"]);

    p5pkg["Perlito5"].v_PKG_NAME = namespace_old;
    p5pkg["Perlito5"].v_VAR      = var_env_js_old;
    p5pkg["Perlito5"].v_STRICT = strict_old;
    return js_code;
}

EOT

} # end of emit_javascript2()

1;


