use v5;

package Perlito5::Javascript2::Runtime;

sub perl5_to_js {
    my ($source, $namespace, $var_env_js) = @_;

    # say "source: [" . $source . "]";

    my    $strict_old         = $Perlito5::STRICT;
    local $Perlito5::VAR      = $var_env_js;
    local $Perlito5::PKG_NAME = $namespace;

    my $match = Perlito5::Grammar->exp_stmts( $source, 0 );

    if ( !$match || $match->{to} != length($source) ) {
        die "Syntax error in eval near pos ", $match->{to};
    }

    my $ast = Perlito5::AST::Do->new(
                block => Perlito5::AST::Lit::Block->new(
                            stmts => $match->{capture},
                         ),
              );

    # say "ast: [" . ast . "]";
    my $js_code = $ast->emit_javascript2(0);
    # say "js-source: [" . $js_code . "]";

    $Perlito5::STRICT   = $strict_old;
    return $js_code;
}

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
    // sub_name can be a function already
    if (typeof sub_name === "function") {
        return sub_name;
    }
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
    if (isa) {
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

function p5sub_exists(name, current_pkg_name) {
    var v = name;
    var pkg_name = v.split(/::/);
    if (pkg_name.length > 1) {
        v = pkg_name.pop();
        pkg_name = pkg_name.join("::");
    }
    else {
        pkg_name = current_pkg_name;
    }
    var c = v.charCodeAt(0);
    if (c < 27) {
        v = String.fromCharCode(c + 64) + v.substr(1);
        pkg_name = 'main';
    }
    return p5pkg.hasOwnProperty(pkg_name) && p5pkg[pkg_name].hasOwnProperty(v) 
}

function p5sub_prototype(name, current_pkg_name) {
    if (typeof name === "function") {
        return name._prototype_;
    }
    var v = name;
    var pkg_name = v.split(/::/);
    if (pkg_name.length > 1) {
        v = pkg_name.pop();
        pkg_name = pkg_name.join("::");
    }
    else {
        pkg_name = current_pkg_name;
    }
    var c = v.charCodeAt(0);
    if (c < 27) {
        v = String.fromCharCode(c + 64) + v.substr(1);
        pkg_name = 'main';
    }
    if (p5pkg[pkg_name].hasOwnProperty(v)) {
        return p5pkg[pkg_name][v]._prototype_
    }
    return p5pkg["Perlito5"].v_PROTO._hash_[name] || p5pkg["Perlito5"].v_CORE_PROTO._hash_[name]
}

function p5scalar_deref(v, current_pkg_name) {
    if (typeof v === "string") {
        var pkg_name = v.split(/::/);
        if (pkg_name.length > 1) {
            v = pkg_name.pop();
            pkg_name = pkg_name.join("::");
        }
        else {
            pkg_name = current_pkg_name;
        }
        var c = v.charCodeAt(0);
        if (c < 27) {
            v = String.fromCharCode(c + 64) + v.substr(1);
            pkg_name = 'main';
        }
        return p5make_package(pkg_name)["v_"+v];
    }
    return v._scalar_;
}

function p5scalar_deref_set(v, n, current_pkg_name) {
    if (typeof v === "string") {
        var pkg_name = v.split(/::/);
        if (pkg_name.length > 1) {
            v = pkg_name.pop();
            pkg_name = pkg_name.join("::");
        }
        else {
            pkg_name = current_pkg_name;
        }
        var c = v.charCodeAt(0);
        if (c < 27) {
            v = String.fromCharCode(c + 64) + v.substr(1);
            pkg_name = 'main';
        }
        p5make_package(pkg_name)["v_"+v] = n;
        return p5pkg[pkg_name]["v_"+v];
    }
    v._scalar_ = n;
    return v._scalar_;
}

function p5global_array(pkg_name, name) {
    v = "List_"+name;
    if (!p5make_package(pkg_name).hasOwnProperty(v)) {
        p5pkg[pkg_name][v] = [];
    }
    return p5pkg[pkg_name][v];
}

function p5global_hash(pkg_name, name) {
    v = "Hash_"+name;
    if (!p5make_package(pkg_name).hasOwnProperty(v)) {
        p5pkg[pkg_name][v] = {};
    }
    return p5pkg[pkg_name][v];
}

p5make_package("main");
p5make_package("Perlito5");
p5pkg["Perlito5"].v_PKG_NAME = "main";
p5pkg["main"]["v_@"] = [];      // $@
p5pkg["main"]["v_|"] = 0;       // $|
p5pkg["main"]["v_/"] = "\n";    // $/
p5pkg["main"]['v_"'] = " ";     // $"
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

var sigils = { '@' : 'List_', '%' : 'Hash_', '$' : 'v_', '&' : '' };

function p5typeglob_set(namespace, name, obj) {
    p5make_package(namespace);
    if ( obj.hasOwnProperty("_ref_") ) {
        if ( obj._ref_ == "HASH" ) {
            p5pkg[namespace][sigils['%'] + name] = obj._hash_;
        }
        else if ( obj._ref_ == "ARRAY" ) {
            p5pkg[namespace][sigils['@'] + name] = obj._array_;
        }
        else if ( obj._ref_ == "SCALAR" ) {
            p5pkg[namespace][sigils['$'] + name] = obj._scalar_;
        }
        else if ( obj._ref_ == "CODE" ) {
            p5pkg[namespace][sigils['&'] + name] = obj._code_;
        }
        else if ( obj._ref_ == "GLOB" ) {
            // TODO
            p5pkg[namespace][name] = obj;
        }
    }
    else {
        p5pkg[namespace][name] = obj;   // native CODE
        // TODO - non-reference
    }
    return p5pkg[namespace][name];  // TODO - return GLOB
}

function p5typeglob_deref_set(v, obj, current_pkg_name) {
    if (typeof v === "string") {
        var pkg_name = v.split(/::/);
        if (pkg_name.length > 1) {
            v = pkg_name.pop();
            pkg_name = pkg_name.join("::");
        }
        else {
            pkg_name = current_pkg_name;
        }
        return p5typeglob_set(pkg_name, v, obj);
    }
    CORE.die(["TODO: can't p5typeglob_deref_set()"]);
}

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

var p5id = Math.floor(Math.random() * 1000000000) + 1000000000;

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

function p5CodeRef(o) {
    this._code_ = o;
    this._ref_ = "CODE";
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
        this[i] = p5incr_(this[i]);
        return this[i];
    }
});
Object.defineProperty( Object.prototype, "p5postincr", {
    enumerable : false,
    value : function (i) {
        var v = this[i];
        this[i] = p5incr_(this[i]);
        return v;
    }
});
Object.defineProperty( Object.prototype, "p5decr", {
    enumerable : false,
    value : function (i) {
        this[i] = p5decr_(this[i]);
        return this[i];
    }
});
Object.defineProperty( Object.prototype, "p5postdecr", {
    enumerable : false,
    value : function (i) {
        var v = this[i];
        this[i] = p5decr_(this[i]);
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

p5_list_of_refs = function(a) {
    // implements \( @a )
    var res = [];
    for (i = 0; i < a.length; i++) {
        res.push(new p5ScalarRef(a[i]));
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
    if (typeof o === "object") {
        if (o instanceof Array) {
            return CORE.join(["", o]);
        }
        if ( o.hasOwnProperty("_ref_") ) {
            var class_name = '';
            if (o._class_ && typeof o._class_._ref_ === "string") {
                // blessed reference
                // test for overload
                var meth = p5method_lookup('(""', o._class_._ref_, {});
                if (meth) {
                    return p5str(meth([o], 1));
                }
                // no overload, strigify the reference instead
                class_name = o._class_._ref_ + '=';
            }
            if (!o._id_) { o._id_ = p5id++ }
            return [class_name, o._ref_, '(0x', o._id_.toString( 16 ), ')'].join('');
        }
    }
    if (typeof o === "function") {
        var class_name = '';
        if (o._class_ && typeof o._class_._ref_ === "string") {
            // blessed reference
            class_name = o._class_._ref_ + '=';
        }
        if (!o._id_) { o._id_ = p5id++ }
        return [class_name, 'CODE(0x', o._id_.toString( 16 ), ')'].join('');
    }
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
    if (typeof o !== "number") {
        var s = p5str(o).trim();
        var s1 = s.substr(0, 3).toUpperCase();
        if ( s1 == "NAN" ) { return NaN };
        if ( s1 == "INF" ) { return Infinity };
        s1 = s.substr(0, 4).toUpperCase();
        if ( s1 == "-NAN" ) { return NaN };
        if ( s1 == "-INF" ) { return -Infinity };
        s1 = parseFloat(s);
        if ( isNaN(s1) ) { return 0 };
        return s1;
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

p5incr_ = function(o) {
    if (typeof o === "number") {
        return o + 1;
    }
    return p5str_inc(p5str(o));
};

p5decr_ = function(o) {
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

p5list_replicate = function(o, n) {
    o = p5list_to_a(o);
    n = p5num(n);
    var out = [];
    for(var i = 0; i < n; i++) {
        for(var j = 0; j < o.length; j++) {
            out.push(o[j]);
        }
    }
    return out;
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

p5negative = function(o) {
    if (o == null) {
        return '-0';
    }
    if (typeof o === "object" && (o instanceof Array)) {
        return -(o.length);
    }
    if (typeof o !== "number") {
        var s = p5str(o);
        s1 = parseFloat(s.trim());
        if ( isNaN(s1) ) {
            var c = s.substr(0, 1);
            if ( c == '+' ) { s = s.substr(1); return '-' + s }
            if ( c == '-' ) { s = s.substr(1); return '+' + s }
            if ( c.length && !c.match(/[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]/) ) {
                if ( s.trim().substr(0,1) == "-" ) { return 0 };
                return '-0';
            };
            return '-' + s
        };
        return -s1;
    }
    return -o;
};

p5qr = function(s, modifier) {
    // TODO
    CORE.die(["qr() not yet implemented"]);
};

p5for = function(namespace, var_name, func, args, cont, label) {
    var _redo = false;
    var v_old = namespace[var_name];
    for(var i = 0; i < args.length; i++) {
        namespace[var_name] = args[i];
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
    namespace[var_name] = v_old;
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

EOT

} # end of emit_javascript2()

1;


