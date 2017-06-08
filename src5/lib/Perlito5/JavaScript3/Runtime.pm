use v5;

package Perlito5::JavaScript3::Runtime;

sub emit_javascript3 {

    return <<'EOT';
//
// lib/Perlito5/JavaScript3/Runtime.js
//
// Runtime for "Perlito" Perl5-in-JavaScript3
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
        var o = List__[0];
        var s = p5str(List__[1]);
        if (o instanceof p5Scalar) {
            o = o.FETCH();
        }
        return o._class_._ref_ == s
    };
    p5pkg.UNIVERSAL.can = function (List__) {
        var o = List__[0];
        var s = p5str(List__[1]);
        if (o instanceof p5Scalar) {
            o = o.FETCH();
        }
        if ( s.indexOf("::") == -1 ) {
            return p5method_lookup(s, o._class_._ref_, {})
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
        this.v = this.message = v;
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

        // TODO - add the other package global variables
        p5pkg[pkg_name]["List_ISA"] = new p5Array([]);
        p5pkg[pkg_name]["v_a"] = new p5Scalar(null);
        p5pkg[pkg_name]["v_b"] = new p5Scalar(null);
        p5pkg[pkg_name]["v__"] = new p5Scalar(null);
        p5pkg[pkg_name]["v_AUTOLOAD"] = new p5Scalar(null);
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
    list.unshift(invocant);

    if (invocant instanceof p5Scalar) {
        // TODO - move p5call() to p5Scalar method
        invocant = invocant.FETCH();
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

    // the invocant doesn't have a class

    if (typeof invocant === "string") {
        var aclass = p5make_package(invocant);
        return p5call(aclass, method, list, p5want);
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
p5pkg["main"]["v_@"]       = new p5Scalar("");  // $@
p5pkg["main"]["v_|"]       = new p5Scalar(0);   // $|
p5pkg["main"]["List_#"]    = new p5Array([]);   // @#
p5scalar_deref_set(String.fromCharCode(15), isNode ? "node.js" : "javascript2");  // $^O
p5pkg["main"]["List_INC"]  = new p5Array([]);
p5pkg["main"]["Hash_INC"]  = new p5Hash({});
p5pkg["main"]["List_ARGV"] = new p5Array([]);
p5pkg["main"]["Hash_ENV"]  = new p5Hash({});
if (isNode) {
    p5pkg["main"]["List_ARGV"] = new p5Array(process.argv.splice(2));
    p5pkg["main"]["Hash_ENV"]  = new p5Hash(process.env);
    p5pkg["main"]["v_$"]       = new p5Scalar(process.pid);
} else if (typeof arguments === "object") {
    p5pkg["main"]["List_ARGV"] = new p5Array(arguments);
}

p5make_package("Perlito5::IO");
p5make_package("Perlito5::Runtime");
p5make_package("Perlito5::Grammar");

function p5typeglob_set(pkg_name, sub_name, func) {
    p5make_package(pkg_name);
    p5pkg[pkg_name][sub_name] = func;
}

function p5set_glob(name, data) {
    if ( name.indexOf("::") == -1 ) {
        p5pkg[ p5pkg["Perlito5"].v_PKG_NAME.FETCH() ][name] = data;
        return data;
    }
    var c = name.split("::");
    s = c.pop(); 
    var pkg = c.join("::");
    p5make_package(pkg);
    p5pkg[pkg][s] = data;
    return data;
}

var sigils = { '@' : 'List_', '%' : 'Hash_', '$' : 'v_' };

function p5set_local(namespace, name, sigil) {
    var vname = sigils[sigil] + name;
    p5LOCAL.push([namespace, vname, namespace[vname]]);

    if (sigil == '$') {
        namespace[vname] = new p5Scalar(null);
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

function p5global(sigil, namespace, name) {
    // TODO - autovivify namespace
    var vname = sigils[sigil] + name;
    var v = p5pkg[namespace][vname];
    if (v != null) {
        return v;
    }
    if (sigil == '$') {
        p5pkg[namespace][vname] = new p5Scalar(null);
    }
    else if (sigil == '@') {
        p5pkg[namespace][vname] = new p5Array([]);
    }
    else if (sigil == '%') {
        p5pkg[namespace][vname] = new p5Hash({});
    }
    return p5pkg[namespace][vname];
}

function p5HashRef(o) {
    this._href_ = o;
    this._ref_ = "HASH";
    this.p5bool = function() { return 1 };
    this.p5string = function() {
        return "HASH(0x0000)";  // TODO
    };
    this.hderef = function() {
        return this._href_;
    };
    this.hset = function(i, v) {
        return this._href_.hset(i, v);
    };
    this.hget = function(i, autoviv) {
        return this._href_.hget(i, autoviv);
    }
}

function p5ArrayRef(o) {
    this._aref_ = o;
    this._ref_ = "ARRAY";
    this.p5bool = function() { return 1 };
    this.p5string = function() {
        return "ARRAY(0x0000)";  // TODO
    };
    this.aderef = function() {
        return this._aref_;
    };
    this.aset = function(i, v) {
        return this._aref_.aset(i, v);
    }
    this.aget = function(i, autoviv) {
        return this._aref_.aget(i, autoviv);
    }
}

function p5ScalarRef(o) {
    this._scalar_ = o;
    this._ref_ = "SCALAR";
    this.p5bool = function() { return 1 };
    this.p5string = function() {
        return "SCALAR(0x0000)";  // TODO
    };
    this.sderef = function() {
        return this._scalar_;
    };
}

function p5GlobRef(o) {
    this._scalar_ = o;
    this._ref_ = "GLOB";
    this.p5bool = function() { return 1 };
    this.p5string = function() {
        return "GLOB(0x0000)";  // TODO
    };
}

function p5Array(o) {
    // TODO - array slice
    this._array_ = o;
    this._ref_ = "";
    this.p5bool = function() {
        return this._array_.length != 0
    };
    this.p5string = function() {
        return '' + this._array_.length;
    };
    this.p5num = function() {
        return this._array_.length;
    };
    this.aset = function(i, v) {
        if (i < 0) {
            i = this._array_.length + i;
        }
        if (this._array_[i] instanceof p5Scalar) {
            this._array_[i].assign(v);
        }
        else {
            if (v instanceof p5Scalar) {
                this._array_[i] = v.FETCH();
            }
            else {
                this._array_[i] = v;
            }
        }
        return v;
    };
    this.aget = function(i, autoviv) {
        if (i < 0) {
            i = this._array_.length + i;
        }
        if (autoviv) {
            if (autoviv == 'lvalue') {
                if (this._array_.length < i) {
                    // don't vivify yet; create a proxy object
                    return new p5ArrayProxy(this, i);
                }
                if (!(this._array_[i] instanceof p5Scalar)) {
                    this._array_[i] = new p5Scalar(this._array_[i]);
                }
            }
            else if (autoviv == 'array') {
                if (!(this._array_[i] instanceof p5ArrayRef) &&
                    !(this._array_[i] instanceof p5Scalar))
                {
                    this._array_[i] = new p5ArrayRef(new p5Array([]));
                }
            }
            else if (autoviv == 'hash') {
                if (!(this._array_[i] instanceof p5HashRef) &&
                    !(this._array_[i] instanceof p5Scalar))
                {
                    this._array_[i] = new p5HashRef(new p5Hash({}));
                }
            }
        }
        return this._array_[i];
    };
    this.get_values = function(o) {
        // add values to the param (a native js list)
        for(var i = 0; i < this._array_.length; i++) {
            o.push(this._array_[i]);
        }
        return o;
    };
    this.get_lvalues = function(o) {
        // add lvalues to the param (a native js list)
        for(var i = 0; i < this._array_.length; i++) {
            o.push(this._array_[i] instanceof p5Scalar ? this._array_[i] : this.aget(i, "lvalue"));
        }
        return o;
    };
    this.assign = function(a) {
        if (a instanceof Array) {
            // TODO - cleanup, this shouldn't happen
            this._array_ = a;
        }
        else {
            this._array_ = a._array_;
        }
        return this;
    };

    // operations that can be tie()
    this.FETCHSIZE = function() {
        return this._array_.length;
    };
    this.PUSH = function(v) {
        for(var i = 0; i < v._array_.length; i++) {
            this._array_.push(v._array_[i] instanceof p5Scalar ? v._array_[i].FETCH() :  v._array_[i]);
        }
        return this._array_.length;
    };
    this.UNSHIFT = function(v) {
        for(var i = v._array_.length-1; i >= 0; i--) {
            this._array_.unshift(v._array_[i] instanceof p5Scalar ? v._array_[i].FETCH() :  v._array_[i]);
        }
        return this._array_.length;
    };
    this.POP = function() {
        if (this._array_.length == null) {
            return null;
        }
        return this._array_.pop();
    };
    this.SHIFT = function(v) {
        if (this._array_.length == null) {
            return null;
        }
        return this._array_.shift();
    };
}

function p5Hash(o) {
    // TODO - hash slice
    this._hash_ = o;
    this._ref_ = "";
    this.p5bool = function() {
        o = this._hash_;
        for (var i in o) {
            return true;
        }
        return false;
    };
    this.p5string = function() {
        return '' + this.p5num() + '/8';
    };
    this.p5num = function() {
        var out = 0;
        for (var i in this._hash_) {
            out++;
        }
        return out;
    };
    this.exists = function(i) {
        return this._hash_.hasOwnProperty(i);
    };
    this.hset = function(i, v) {
        if (this._hash_[i] instanceof p5Scalar) {
            this._hash_[i].assign(v);
        }
        else {
            if (v instanceof p5Scalar) {
                this._hash_[i] = v.FETCH();
            }
            else {
                this._hash_[i] = v;
            }
        }
        return v;
    };
    this.hget = function(i, autoviv) {
        if (autoviv) {
            if (autoviv == 'lvalue') {
                if (! this._hash_.hasOwnProperty(i)) {
                    // don't autovivify yet; create a proxy object
                    return new p5HashProxy(this, i);
                }
                if (!(this._hash_[i] instanceof p5Scalar)) {
                    this._hash_[i] = new p5Scalar(this._hash_[i]);
                }
            }
            else if (autoviv == 'array') {
                if (!(this._hash_[i] instanceof p5ArrayRef) &&
                    !(this._hash_[i] instanceof p5Scalar))
                {
                    this._hash_[i] = new p5ArrayRef(new p5Array([]));
                }
            }
            else if (autoviv == 'hash') {
                if (!(this._hash_[i] instanceof p5HashRef) &&
                    !(this._hash_[i] instanceof p5Scalar))
                {
                    this._hash_[i] = new p5HashRef(new p5Hash({}));
                }
            }
        }
        return this._hash_[i];
    };
    this.get_values = function(o) {
        // add a native list of values to the param
        for (var i in this._hash_) {
            o.push(i);
            o.push(this._hash_[i]);
        }
        return o;
    };
    this.get_lvalues = function(o) {
        // add a native list of lvalues to the param
        for (var i in this._hash_) {
            o.push(i);
            o.push(this._hash_[i] instanceof p5Scalar ? this._hash_[i] : this.hget(i, "lvalue"));
        }
        return o;
    };
    this.assign = function(h) {
        if (h instanceof p5Hash) {
            this._hash_ = h._hash_;
        }
        else {
            // TODO - cleanup, this shouldn't happen
            this._hash_ = h;
        }
        return this;
    }
}

function p5Scalar(o) {
    this._v_ = o;
    this._ref_ = "";

    // be a value
    this.p5bool = function() {
        return p5bool(this._v_);
    };
    this.p5string = function() {
        return p5str(this._v_);
    };
    this.p5num = function() {
        return p5num(this._v_);
    };
    this.p5code = function() {
        return p5code(this._v_);
    };
    this.p5incr = function() {
        this._v_ = p5incr(this._v_);
        return this._v_;
    };
    this.p5postincr = function() {
        var v = this._v_;
        this._v_ = p5incr(this._v_);
        return v;
    };
    this.p5decr = function() {
        this._v_ = p5decr(this._v_);
        return this._v_;
    };
    this.p5postdecr = function() {
        var v = this._v_;
        this._v_ = p5decr(this._v_);
        return v;
    };

    // be a scalar ref
    this.sderef = function(i) {
        // TODO - autovivify scalar (with proxy object?)
        return this._v_.sderef();
    };

    // be an array ref
    this.aderef = function() {
        // TODO - autovivify array (with proxy object?)
        return this._v_.aderef();
    };
    this.aget = function(i, autoviv) {
        // TODO - autovivify array (with proxy object?)
        if (this._v_ == null) {
            this._v_ = new p5ArrayRef(new p5Array([]));
        }
        return this._v_.aget(i, autoviv);
    };
    this.aset = function(i, v) {
        if (this._v_ == null) {
            this._v_ = new p5ArrayRef(new p5Array([]));
        }
        return this._v_.aset(i, v);
    };

    // be a hash ref
    this.hderef = function() {
        // TODO - autovivify hash (with proxy object?)
        if (this._v_ == null) {
            this._v_ = new p5HashRef(new p5Hash([]));
        }
        return this._v_.hderef();
    };
    this.hget = function(i, autoviv) {
        // TODO - autovivify hash (with proxy object?)
        if (this._v_ == null) {
            this._v_ = new p5HashRef(new p5Hash([]));
        }
        return this._v_.hget(i, autoviv);
    }
    this.hset = function(i, v) {
        if (this._v_ == null) {
            this._v_ = new p5HashRef(new p5Hash([]));
        }
        return this._v_.hset(i, v);
    }

    // be a container
    this.get_values = function(o) {
        // add a native list of values to the param
        o.push(this);
        return o;
    };
    this.get_lvalues = function(o) {
        // add a native list of lvalues to the param
        o.push(this);
        return o;
    };
    this.assign = function(v) {
        if (v instanceof p5Scalar) {
            this._v_ = v.FETCH();
        }
        else {
            this._v_ = v;
        }
        return this;
    };

    // operations that can be tie()
    this.FETCH = function() {
        // not an lvalue
        return this._v_;
    };
}


function p5HashProxy(h, k) {
    this._hashobj_ = h;
    this._key_ = k;
    this._v_ = null;
    this.assign = function(v) {
        // write-through; alternately, use read-through
        if (v instanceof p5Scalar) {
            this._v_ = v.FETCH();
        }
        else {
            this._v_ = v;
        }
        return this._hashobj_.hset(this._key_, this._v_);
    }
}
p5HashProxy.prototype = new p5Scalar();


function p5ArrayProxy(a, k) {
    this._arrayobj_ = a;
    this._key_ = k;
    this._v_ = null;
    this.assign = function(v) {
        // write-through; alternately, use read-through
        if (v instanceof p5Scalar) {
            this._v_ = v.FETCH();
        }
        else {
            this._v_ = v;
        }
        return this._arrayobj_.aset(this._key_, this._v_);
    }
}
p5ArrayProxy.prototype = new p5Scalar();


p5param_list = function() {
    var res = [];
    for (i = 0; i < arguments.length; i++) {
        if (arguments[i] == null) {
            res.push(null)
        }
        else if (arguments[i].hasOwnProperty("get_lvalues")) {
            // container
            arguments[i].get_lvalues(res);
        }
        else if (arguments[i] instanceof Array) {
            // js Array -- possibly generated by p5context()
            // maybe too late to get lvalues -- needs more testing
            // this doesn't handle nested Array
            o = arguments[i];
            for (j = 0; j < o.length; j++) {
                res.push(o[j]);
            }
        }
        else {
            // non-container
            res.push(arguments[i]);
        }
    }
    return res;
};

p5list_to_a = function() {
    var res = [];
    for (i = 0; i < arguments.length; i++) {
        if (arguments[i] == null) {
            res.push(null)
        }
        else if (arguments[i].hasOwnProperty("get_values")) {
            // container
            arguments[i].get_values(res);
        }
        else if (arguments[i] instanceof Array) {
            // js Array -- possibly generated by p5context()
            o = arguments[i];
            for (j = 0; j < o.length; j++) {
                res.push(o[j]);
            }
        }
        else {
            // non-container
            res.push(arguments[i]);
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

if (isNode) {
    var fs = require("fs");
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

p5code = function(o) {
    if (typeof o === "function") {
        return o;
    }
    return o.p5code();
};

p5str = function(o) {
    if (o == null) {
        return "";
    }
    if (typeof o === "object" && (o instanceof Array)) {
        return CORE.join(["", o]);
    }
    if (typeof o.p5string === "function") {
        return o.p5string();
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
    if (typeof o.p5num === "function") {
        return o.p5num();
    }
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
        if (typeof o.p5bool === "function") {
            return o.p5bool();
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
    namespace["v__"] = new p5Scalar(null);

    for(var i = 0; i < args.length; i++) {
        namespace["v__"].assign(args[i]);   // ??? - should this be a bind?
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
    var _arg  = new p5Scalar(null);
    for(var i = 0; i < args.length; i++) {
        try {
            _arg.assign(args[i]);
            func(_arg)
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
    namespace["v__"] = new p5Scalar(null);

    var out = [];
    for(var i = 0; i < args.length; i++) {
        namespace["v__"].assign(args[i]);
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
    namespace["v__"] = new p5Scalar(null);

    var out = [];
    for(var i = 0; i < args.length; i++) {
        namespace["v__"].assign(args[i]);
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
    namespace["v_a"] = new p5Scalar(null);
    namespace["v_b"] = new p5Scalar(null);

    var out = 
        func == null
        ? args.sort()
        : args.sort(
            function(a, b) {
                namespace["v_a"].assign(a);
                namespace["v_b"].assign(b);
                return func(0);
            }
        );
    namespace["v_a"] = a_old;
    namespace["v_b"] = b_old;
    return out;
};

perl5_to_js = function( source, namespace, var_env_js, p5want ) {
    // CORE.say(["source: [" + source + "]"]);

    var namespace_old = p5global("$", "Perlito5", "PKG_NAME").FETCH();
    p5pkg["Perlito5"].v_PKG_NAME.assign(namespace);

    match = p5call(p5pkg["Perlito5::Grammar"], "exp_stmts", [source, 0]);

    if ( !p5bool(match) || p5str(match.hget("to")) != source.length ) {
        CORE.die(["Syntax error in eval near pos ", match.hget("to") ]);
    }

    ast = p5pkg.CORE.bless([
        new p5HashRef(new p5Hash({
            code: "do",
            arguments: [ p5pkg.CORE.bless([
                        new p5HashRef(new p5Hash({
                            stmts:   p5pkg["Perlito5::Match"].flat([match])
                        })),
                        p5pkg["Perlito5::AST::Block"]
                    ]) ]
        })),
        p5pkg["Perlito5::AST::Apply"]
    ]);

    // CORE.say(["ast: [" + ast + "]"]);
    js_code = p5call(ast, "emit_javascript3", [0, p5want]);
    // CORE.say(["js-source: [" + js_code + "]"]);

    p5pkg["Perlito5"].v_PKG_NAME.assign(namespace_old);
    return js_code;
}

EOT

} # end of emit_javascript3()

1;


