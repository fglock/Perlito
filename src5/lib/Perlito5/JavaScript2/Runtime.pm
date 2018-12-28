use v5;

package Perlito5::JavaScript2::Runtime;

use Perlito5::Runtime::Sprintf;

sub perl5_to_js {
    my ($source, $namespace, $want, $scalar_hints, $hash_hints, $scope_js) = @_;

    # say "source: [" . $source . "]";

    local $_;
    local ${^GLOBAL_PHASE};
    local $^H = $scalar_hints;
    local %^H = %$hash_hints;
    local @Perlito5::BASE_SCOPE = ($scope_js->[0]);
    local @Perlito5::SCOPE_STMT;
    local $Perlito5::CLOSURE_SCOPE = 0;
    local $Perlito5::PKG_NAME = $namespace;
    local @Perlito5::UNITCHECK_BLOCK;
    # warn "in eval enter\n";
    # warn "External scope ", Perlito5::Dumper::Dumper($scope_js);
    # warn "BASE_SCOPE ", Perlito5::Dumper::Dumper($Perlito5::BASE_SCOPE);
    # warn "SCOPE_STMT ", Perlito5::Dumper::Dumper(\@Perlito5::SCOPE_STMT);

    my $match = Perlito5::Grammar::exp_stmts( $source, 0 );

    if ( !$match || $match->{to} != length($source) ) {
        die "Syntax error in eval near pos ", $match->{to};
    }

    my $ast = Perlito5::AST::Apply->new(
                code => 'do',
                arguments => [ Perlito5::AST::Block->new(
                            stmts => $match->{capture},
                         ) ],
              );

    # use lexicals from BEGIN scratchpad
    $ast = $ast->emit_begin_scratchpad();

    # say "ast: [" . ast . "]";
    my $js_code = $ast->emit_javascript2(0, $want);

    Perlito5::set_global_phase("UNITCHECK");
    $_->() while $_ = shift @Perlito5::UNITCHECK_BLOCK;

    # warn "in eval BASE_SCOPE exit: ", Perlito5::Dumper::Dumper($Perlito5::BASE_SCOPE);
    if ($Perlito5::JavaScript::DEBUG) {
        # "-JS DEBUG" switch in the command line
        print $js_code, "\n\n";
    }
    return $js_code;
}

sub eval_ast {
    my ($ast) = @_;
    my $want = 0;

    # use lexicals from BEGIN scratchpad
    $ast = $ast->emit_begin_scratchpad();

    my $js_code = $ast->emit_javascript2(0, $want);
    # say STDERR "js-source: [" . $js_code . "]";
    Perlito5::set_global_phase("UNITCHECK");
    $_->() while $_ = shift @Perlito5::UNITCHECK_BLOCK;
    # warn "in eval BASE_SCOPE exit: ", Perlito5::Dumper::Dumper($Perlito5::BASE_SCOPE);
    if ($Perlito5::JavaScript::DEBUG) {
        # "-JS DEBUG" switch in the command line
        print $js_code, "\n\n";
    }
    $_ = $js_code;
    return JS::inline('eval("(function(){" + p5pkg.main.v__ + "})()")');
}

sub emit_javascript2 {

    return <<'EOT';
//
// lib/Perlito5/JavaScript2/Runtime.js
//
// Runtime for "Perlito" Perl5-in-JavaScript2
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

"use strict";
var isNode = typeof require != "undefined";

if (typeof p5pkg !== "object") {
    var p5pkg = {};
    var p5LOCAL = [];

    var universal = function () {};
    p5pkg.UNIVERSAL = new universal();
    p5pkg.UNIVERSAL._ref_ = "UNIVERSAL";
    p5pkg.UNIVERSAL.isa = function (List__) {
        var o = List__[0];
        var s = List__[1];
        var clas;
        if (typeof o === "string") {
            clas = p5pkg[o];
        }
        else {
            clas = o._class_;
        }
        if (!clas) {
            return false;
        }
        if (clas._ref_ == s) {
            return true;
        }
        var isa = clas.List_ISA;
        if (isa) {
            for (var i = 0; i < isa.length; i++) {
                if (isa[i] == s) {
                    return true;
                }
                if (p5pkg.UNIVERSAL.isa( isa[i], s )) {
                    return true;
                }
            }
        }
        return false;
    };
    p5pkg.UNIVERSAL.can = function (List__) {
        var o = List__[0];
        var s = List__[1];
        if (typeof o === "string") {
            return p5method_lookup(s, o, {})
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

    var p5_error = function (type, v) {
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
        tmp.prototype = p5pkg.CORE;
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

function p5method_not_found(method, class_name) {
    return "Can't locate object method \""
        + method + "\" via package \"" + class_name + "\" (perhaps you forgot to load \""
        + class_name + "\"?)";
}

function p5call(invocant, method, list, p5want) {
    var invocant_original = invocant;
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

        var invocant_ref = invocant._class_._ref_;
        var m = p5method_lookup(method, invocant_ref, {});
        if (m) {
            return m(list, p5want)
        }
        // method can have an optional namespace
        var pkg_name = method.split(/::/);
        var name;
        if (pkg_name.length == 1) {
            pkg_name = invocant_ref.split(/::/);
            name = method;
        }
        else {
            name = pkg_name.pop();
        }
        if (pkg_name.length > 1) {
            if (pkg_name.length > 1 && pkg_name[0] === "main") {
                // normalize main::x::get to x::get
                pkg_name.shift();
            }
            pkg_name = pkg_name.join("::");
            m = p5method_lookup(name, pkg_name, {});
            if (m) {
                return m(list, p5want)
            }
            p5pkg.CORE.die([p5method_not_found(name, pkg_name)]);
        }

        if (method == "print" || method == "printf" || method == "say" || method == "close") {
            list.shift();
            return p5pkg['Perlito5::IO'][method]( invocant_original, list, p5want);
        }

        if (method.substr(0, 1) != "("
         && method != "import"
         && method != "unimport"
         && method != "isa"
         && method != "can"
        ) {
            pkg_name = p5get_class_for_method('AUTOLOAD', invocant._class_._ref_, {}) || p5get_class_for_method('AUTOLOAD', "UNIVERSAL", {});
            if (pkg_name) {
                p5pkg[pkg_name]["v_AUTOLOAD"] = invocant._class_._ref_ + "::" + method;
                return p5pkg[pkg_name]["AUTOLOAD"](list, p5want);
            }
        }
        p5pkg.CORE.die([p5method_not_found(method, invocant._class_._ref_)]);
    }
    p5pkg.CORE.die(["Can't call method ", method, " on unblessed reference"]);
}

function p5cget(namespace, name) {
    if(p5pkg[namespace].hasOwnProperty(name)) {
        return p5pkg[namespace][name]
    }
    if(p5pkg[namespace].hasOwnProperty("AUTOLOAD")) {
        p5pkg[namespace]["v_AUTOLOAD"] = namespace + "::" + name;
        return p5pkg[namespace]["AUTOLOAD"]
    }
    p5pkg.CORE.die(["Undefined subroutine &" + namespace + "::" + name]);
}

function p5cget_by_name(namespace, name) {
    // name can be a function already
    if (typeof name === "function") {
        return name;
    }
    // name can have an optional namespace
    var parts = name.split(/::/);
    if (parts.length > 1) {
        name = parts.pop();
        namespace = parts.join("::");
    }
    if(p5pkg[namespace].hasOwnProperty(name)) {
        return p5pkg[namespace][name]
    }
    if(p5pkg[namespace].hasOwnProperty("AUTOLOAD")) {
        p5pkg[namespace]["v_AUTOLOAD"] = namespace + "::" + name;
        return p5pkg[namespace]["AUTOLOAD"]
    }
    p5pkg.CORE.die(["Undefined subroutine &" + namespace + "::" + name]);
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
        pkg_name = 'main';
    }
    return p5pkg.hasOwnProperty(pkg_name) && p5pkg[pkg_name].hasOwnProperty(v) 
}

function p5sub_prototype(name, current_pkg_name) {
    if (!name) {
        return null;
    }
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
        pkg_name = 'main';
    }
    if (p5pkg.hasOwnProperty(pkg_name) && p5pkg[pkg_name].hasOwnProperty(v)) {
        return p5pkg[pkg_name][v]._prototype_
    }
    return p5pkg["Perlito5"].v_PROTO._hash_[name] || p5pkg["Perlito5"].v_CORE_PROTO._hash_[name]
}

function p5scalar_deref(v, current_pkg_name, autoviv_type) {
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
            pkg_name = 'main';
        }
        var name = "v_"+v;
        if (!p5make_package(pkg_name)[name]) {
            if (autoviv_type == 'array') {
                p5pkg[pkg_name][name] = new p5ArrayRef([]);
            }
            else if (autoviv_type == 'hash') {
                p5pkg[pkg_name][name] = new p5HashRef([]);
            }
            else if (autoviv_type == 'scalar') {
                p5pkg[pkg_name][name] = new p5ScalarRef([]);
            }
        }
        return p5pkg[pkg_name][name];
    }
    if (!v._scalar_) {
        CORE.die(["not a SCALAR reference"]);
        // if (autoviv_type == 'array') {
        //     v._scalar_ = new p5ArrayRef([]);
        // }
        // else if (autoviv_type == 'hash') {
        //     v._scalar_ = new p5HashRef([]);
        // }
        // else if (autoviv_type == 'scalar') {
        //     v._scalar_ = new p5ScalarRef([]);
        // }
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
            pkg_name = 'main';
        }
        var name = "v_"+v;
        p5make_package(pkg_name)[name] = n;
        return p5pkg[pkg_name][name];
    }
    v._scalar_ = n;
    return v._scalar_;
}

function p5array_deref(v, current_pkg_name) {
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
            pkg_name = 'main';
        }
        var name = "List_"+v;
        if (!p5make_package(pkg_name)[name]) {
                p5pkg[pkg_name][name] = [];
        }
        return p5pkg[pkg_name][name];
    }
    return v._array_;
}

function p5array_deref_set(v, list, current_pkg_name) {
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
            pkg_name = 'main';
        }
        var name = "List_"+v;
        if (!p5make_package(pkg_name)[name]) {
                p5pkg[pkg_name][name] = [];
        }
        p5pkg[pkg_name][name] = list;
        return p5pkg[pkg_name][name];
    }
    v._array_ = list;
    return v._array_;
}

function p5hash_deref(v, current_pkg_name) {
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
            pkg_name = 'main';
        }
        var name = "Hash_"+v;
        if (!p5make_package(pkg_name)[name]) {
                p5pkg[pkg_name][name] = [];
        }
        return p5pkg[pkg_name][name];
    }
    return v._hash_;
}

// regex globals
p5make_package("Regex");
var p5_last_regex = new RegExp("", "");
var p5_regex_capture = [];

p5make_package("main");
p5make_package("Perlito5");
p5pkg["Perlito5"].v_PKG_NAME = "main";
p5make_package("main::STDIN").file_handle = { id : 0, readline_buffer : '' };
p5make_package("main::STDOUT").file_handle = { id : 1 };
p5make_package("main::STDERR").file_handle = { id : 2 };
p5make_package("main::STDIN")['List_ISA'] = ['Perlito5::IO'];
// p5make_package("main::STDOUT")['List_ISA'] = ['Perlito5::IO'];
// p5make_package("main::STDERR")['List_ISA'] = ['Perlito5::IO'];
p5make_package("ARGV").file_handle = { id : null };
p5make_package("main")["STDOUT"] = p5pkg["main::STDOUT"];
p5make_package("main")["STDERR"] = p5pkg["main::STDERR"];
p5make_package("main")["STDIN"] = p5pkg["main::STDIN"];
p5pkg["STDOUT"] = p5pkg["main::STDOUT"];
p5pkg["STDERR"] = p5pkg["main::STDERR"];
p5pkg["STDIN"] = p5pkg["main::STDIN"];
p5pkg["Perlito5"].v_SELECT = "main::STDOUT";
p5pkg["main"]["v_@"] = [];      // $@
p5pkg["main"]["v_|"] = 0;       // $|
p5pkg["main"]["v_/"] = "\n";    // $/
p5pkg["main"]['v_"'] = " ";     // $"
p5pkg["main"]["List_#"] = [];   // @#
p5scalar_deref_set(String.fromCharCode(15), isNode ? "node.js" : "javascript2");  // $^O
p5pkg["main"]["List_INC"] = [];
p5pkg["main"]["Hash_INC"] = { "strict.pm" : "strict.pm", "feature.pm" : "feature.pm", "warnings.pm" : "warnings.pm" };
p5pkg["main"]["List_ARGV"] = [];
p5pkg["main"]["Hash_ENV"] = {};
p5pkg["main"]["Hash_SIG"] = {};
if (isNode) {
    p5pkg["main"]["List_ARGV"] = process.argv.splice(2);

    p5pkg["main"]["Hash_ENV"] = {};
    for (var e in process.env) p5pkg["main"]["Hash_ENV"][e] = process.env[e];

    p5pkg["main"]["v_$"] = process.pid;   // $$
    p5scalar_deref_set(String.fromCharCode(24), process.argv[0]);  // $^X
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

function p5cleanup_local(idx, value) {
    while (p5LOCAL.length > idx) {
        var l = p5LOCAL.pop();
        l();
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


var p5context = function(List__, p5want) {
    if (p5want) {
        return p5list_to_a(List__);
    }
    // scalar: return the last value
    var o = List__;
    if (o instanceof Array) {
        o =   o.length
            ? o[o.length-1]
            : null;
    }
    if (o instanceof Array) {
        o =   o.length
    }
    return o;
}

var p5list_to_a = function(args) {
    var res = [];
    for (var i = 0; i < args.length; i++) {
        var o = args[i];
        if  (  o == null
            || o._class_    // perl5 blessed reference
            || o._ref_      // perl5 un-blessed reference
            )
        {
            res.push(o);
        }
        else if (o instanceof Array) {
            // perl5 array
            for (var j = 0; j < o.length; j++) {
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

var p5_list_of_refs = function(a) {
    // implements \( @a )
    var res = [];
    for (var i = 0; i < a.length; i++) {
        res.push(new p5ScalarRef(a[i]));
    }
    return res;
};

var p5a_to_h = function(a) {
    var res = {};
    for (var i = 0; i < a.length; i+=2) {
        res[p5str(a[i])] = a[i+1];
    }
    return res;
};

var p5idx = function(a, i) {
    return i >= 0 ? i : a.length + i
};

var p5smrt_scalar = function(a1, a2) {
    if (a2 == null) {
        return a1 == null;
    }
    if (typeof a2 == "string") {
        return p5str(a1) == a2;
    }
    if (typeof a2 == "number") {
        return p5num(a1) == a2;
    }
    CORE.die("Not implemented: smartmatch operator with argument type '", (typeof a2), "'");
};

var p5refaddr = function(o) {
    if (o == null) {
        return null;
    }
    if (typeof o === "object") {
        if (o instanceof Array) {
            return null;
        }
        if ( o.hasOwnProperty("_ref_") ) {
            if (!o._id_) { o._id_ = p5id++ }
            return o._id_;
        }
    }
    if (typeof o === "function") {
        if (!o._id_) { o._id_ = p5id++ }
        return o._id_;
    }
    return null;
};

var p5str = function(o) {
    if (o == null) {
        return "";
    }
    if (typeof o === "object") {
        if (o instanceof Array) {
            return CORE.join([""].concat(o));
        }
        if ( o.hasOwnProperty("_ref_") ) {
            var class_name = '';
            if (o._class_ && typeof o._class_._ref_ === "string") {
                // blessed reference
                // test for overload
                var meth = p5method_lookup('(""', o._class_._ref_, {});
                if (meth) {
                    return p5str(meth([o], 0));
                }
                // TODO - test the "fallback" flag
                meth = p5method_lookup('(0+', o._class_._ref_, {});
                if (meth) {
                    return p5str(meth([o], 0));
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

var p5num = function(o) {
    if (o == null) {
        return 0;
    }
    if (typeof o === "object") {
        if (o instanceof Array) {
            return o.length;
        }
        if ( o.hasOwnProperty("_ref_") ) {
            if (o._class_ && typeof o._class_._ref_ === "string") {
                // blessed reference
                // test for overload
                var meth = p5method_lookup('(0+', o._class_._ref_, {});
                if (meth) {
                    return p5num(meth([o], 0));
                }
                // TODO - test the "fallback" flag
                meth = p5method_lookup('(""', o._class_._ref_, {});
                if (meth) {
                    return p5num(meth([o], 0));
                }
            }
        }
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

var p5bool = function(o) {
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

var p5incr_ = function(o) {
    if (typeof o === "number") {
        return o + 1;
    }
    return p5str_inc(p5str(o));
};

var p5decr_ = function(o) {
    if (typeof o === "number") {
        return o - 1;
    }
    return p5num(o) - 1;
};

var p5modulo = function(o, k) {
    var m = o % k;
    if ( k < 0 && m > 0 ) {
        m = m + k;
    }
    else if ( k > 0 && m < 0 ) {
        m = m + k;
    }
    return m;
};

var p5shift_left = function(o, k) {
    return k < 31 ? o << k : o * Math.pow(2, k);
};

var p5and = function(a, fb) {
    if (p5bool(a)) {
        return fb();
    }
    return a;
};

var p5or = function(a, fb) {
    if (p5bool(a)) {
        return a;
    }
    return fb();
};

var p5defined_or = function(a, fb) {
    if (a == null) {
        return fb();
    }
    return a;
};

var p5xor = function(a, fb) {
    return p5bool(a) ? !p5bool(fb()) : fb()
};

var p5cmp = function(a, b) {
    return a > b ? 1 : a < b ? -1 : 0 
};

var p5complement = function(a) {
    return a < 0 ? ~a : 4294967295 - a
    // return a < 0 ? ~a : 18446744073709551615 - a
};

var p5str_replicate = function(o, n) {
    n = Math.floor(n);
    return n > 0 ? Array(n + 1).join(o) : "";
};

var p5list_replicate = function(o, n, want) {
    o = p5list_to_a([o]);
    n = p5num(n);

    if (!want) {
        return p5str_replicate(o.pop(), n);   // scalar context
    }

    var out = [];
    for(var i = 0; i < n; i++) {
        for(var j = 0; j < o.length; j++) {
            out.push(o[j]);
        }
    }
    return (want ? out : out.length)
};

var p5list_slice = function(o, ix, want) {
    var out = [];
    for (var i=0, l=ix.length; i<l; ++i) {
        if (ix[i] < o.length) {
            out[i] = o[ix[i]];
        }
    }
    if (want) { return out }
    return out.length ? out[out.length-1] : null;
}

var p5hash_slice = function(o, ix, want) {
    var out = [];
    for (var i=0, l=ix.length; i<l; ++i) {
        out.push(ix[i]);
        out.push(o[ix[i]]);
    }
    if (want) { return out }
    return out.length ? out[out.length-1] : null;
}

var p5list_lookup_slice = function(o, ix, want) {
    var out = [];
    for (var i=0, l=ix.length; i<l; ++i) {
        out[i] = o[ix[i]];
    }
    if (want) { return out }
    return out.length ? out[out.length-1] : null;
}

var p5hash_lookup_slice = function(o, ix, want) {
    var out = [];
    for (var i=0, l=ix.length; i<l; ++i) {
        out.push(ix[i]);
        out.push(o[ix[i]]);
    }
    if (want) { return out }
    return out.length ? out[out.length-1] : null;
}

var p5str_inc = function(s) {
    if (s.length < 2) {
        if ((s >= "0" && s <= "8") || (s >= "A" && s <= "Y") || (s >= "a" && s <= "y")) {
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
    var c0 = s.substr(0, 1);
    if (c0 >= "0" && c0 <= "9") {
        return p5str(p5num(s)+1);
    }
    var c = p5str_inc(s.substr(s.length-1, 1));
    if (c.length == 1) {
        return s.substr(0, s.length-1) + c;
    }
    return p5str_inc(s.substr(0, s.length-1)) + c.substr(c.length-1, 1);
};

var p5looks_like_number = function(a) {
    if (typeof a === "number") {
        return 1;
    }
    a = a.trim();
    var s1 = a.toUpperCase();
    if ( s1 == "NAN" || s1 == "INF" || s1 == "-NAN" || s1 == "-INF" ) {
        return 1
    };
    if (s1.match(/^[\+\-]?[0-9]+\.?(?:E[-+]?[0-9]+)?$/)) {          // 999 999.
        return 1;
    }
    if (s1.match(/^[\+\-]?[0-9]*\.[0-9]+(?:E[-+]?[0-9]+)?$/)) {    // 999.999 .999
        return 1;
    }
    return 0;
}

var p5range_state = {};
var p5range = function(a, b, p5want, id, three_dots) {
    if (p5want) {
        // list context
        var tmp = [];
        if (typeof a === "number" || typeof b === "number") {
            a = p5num(a);
            b = p5num(b);
            if (isNaN(a) || isNaN(b) || a == Infinity || b == Infinity) {
                p5pkg.CORE.die(["Range iterator outside integer range"]);
            }
            a = CORE.int([a]);
            while (a <= b) {
                tmp.push(a);
                a++;
            }
        }
        else {
            a = p5str(a);
            b = p5str(b);
            if (a == '') {
                return [a];
            }

            if (a.substr(0, 1) != '0' && p5looks_like_number(a) && p5looks_like_number(b)) {
                // both sides look like number
                return p5range(p5num(a), p5num(b), p5want, id, three_dots)
            }

            // If the initial value specified isn't part of a magical increment sequence
            // (that is, a non-empty string matching /^[a-zA-Z]*[0-9]*\z/ ),
            // only the initial value will be returned.
            if (!a.match(/^[a-zA-Z]*[0-9]*$/)) {
                if (a.length > b.length) {
                    return []
                }
                return [a]
            }

            while (  (a.length < b.length)
                  || (a.length == b.length && a <= b) ) {
                tmp.push(a);
                a = p5incr_(a);
            }
        }
        return tmp;
    }
    // flip-flop operator
    var v;
    if (p5range_state[id]) {
        v = ++p5range_state[id];
        if (p5bool(b)) {
            p5range_state[id] = 0;
            v = v + "E0";
        }
        return v;
    }
    else {
        p5range_state[id] = 0;
        if (p5bool(a)) {
            p5range_state[id]++;
            v = p5range_state[id];
        }
        if (v && !three_dots && p5bool(b)) {
            p5range_state[id] = 0;
            v = v + "E0";
        }
        return v;
    }
};

var p5negative = function(o) {
    if (o == null) {
        return '-0';
    }
    if (typeof o === "object" && (o instanceof Array)) {
        return -(o.length);
    }
    if (typeof o !== "number") {
        var s = p5str(o);
        var c = s.substr(0, 1);
        if ( c == '+' ) { s = s.substr(1); return '-' + s }
        if ( c == '-' ) { s = s.substr(1); return '+' + s }
        var s1 = parseFloat(s.trim());
        if ( isNaN(s1) ) {
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

function p5regex_compile (s, flags) {
    var flag_x = false;
    var flag_xx = false;
    var flag_s = false;
    var flag_i = false;
    if (flags.indexOf("s") != -1) {
        flags = flags.replace("s", "");
        flag_s = true;
    }
    if (flags.indexOf("xx") != -1) {
        flags = flags.replace("xx", "");
        flag_x = true;
        flag_xx = true;
    }
    if (flags.indexOf("x") != -1) {
        flags = flags.replace("x", "");
        flag_x = true;
    }
    var cc = s.split("");
    var out = [];
    var is_char_class = false;
    var is_comment = false;
    for(var i = 0; i < cc.length; i++) {
        var c = cc[i];
        if (flag_x) {
            if (c == " " && flag_xx )        { continue }
            if (c == " " && !is_char_class ) { continue }
            if (c == "#" && !is_char_class ) { is_comment = true;  continue }
            if (c == "\n" && is_comment )    { is_comment = false; continue }
            if (is_comment)                  { continue }
        } 
        if (c == "\\") {
            out.push(c);
            i++;
            if (i < cc.length) {
                out.push(cc[i]);
            }
            continue;
        }
        if (flag_s) {
            if (c == "." && !is_char_class ) { out.push("[\\S\\s]"); continue }
        }
        if (flag_i && c.toUpperCase() != c.toLowerCase()) {
            if (is_char_class) {
                out.push(c.toUpperCase() + c.toLowerCase()); continue;
            }
            out.push("[" + c.toUpperCase() + c.toLowerCase() + "]"); continue;
        }
        if (c == "(" && !is_char_class ) {
            if (i+1 < cc.length && cc[i+1] == "?") {
                var flag = true;
                while (i+2 < cc.length && (cc[i+2] == "-" || cc[i+2] == "x" || cc[i+2] == "i" || cc[i+2] == "s") ) {
                    // TODO - restore flags at end of pattern group
                    if (cc[i+2] == "-") { flag   = false }    // (?-x) (?-x:
                    if (cc[i+2] == "x") { flag_x = flag  }    // (?x) (?x:
                    if (cc[i+2] == "i") { flag_i = flag  }    // (?i) (?i:
                    if (cc[i+2] == "s") { flag_s = flag  }    // (?s) (?s:
                    if (cc[i+2] == "x" && i+3 < cc.length && cc[i+3] == "x") { flag_xx = flag  }  // (?xx) (?xx:
                    i++;
                }
                if (i+2 < cc.length && cc[i+2] == ")") {
                    i = i + 2;
                    continue;
                }
                if (i+2 < cc.length && cc[i+2] == ":") {
                    i = i + 2;
                    out.push("(?:");
                    continue;
                }
            }
        }
        if (c == ")" && !is_char_class ) {
            // TODO - restore flags at end of pattern group
        }
        if (c == "[")                    { is_char_class = true }
        if (c == "]" && is_char_class )  { is_char_class = false }
        out.push(c);
    }
    var sout = out.join("");
    return new RegExp(sout, flags);
}

var p5qr = function(search, modifier) {
    // TODO - "Regex" stringification
    var re = p5regex_compile(search, modifier);
    return CORE.bless([(new p5ScalarRef(re)), 'Regex']);
};

var p5m = function(s, search, modifier, want) {
    // TODO - captures
    var re;
    if (search.hasOwnProperty('_scalar_')) {
        // search is a Regex object
        re = search._scalar_;
    }
    else {
        re = p5regex_compile(search, modifier);
    }

    p5_regex_capture = [];
    var res = [];
    var myArray;
    while ((myArray = re.exec(s)) !== null) {
        var m = myArray.shift();
        if (myArray.length) {
            res = res.concat(myArray);
            p5_regex_capture = p5_regex_capture.concat(myArray);
        }
        else {
            res.push(m);
        }
        if (re.lastIndex == 0) {
            return (want ? res : res.length)
        }
    }
    return (want ? res : res.length)
};

var p5s = function(s, search, fun_replace, modifier, want) {
    // TODO - captures
    var count = null;
    var re;
    if (search.hasOwnProperty('_scalar_')) {
        // search is a Regex object
        re = search._scalar_;
    }
    else {
        re = p5regex_compile(search, modifier);
    }

    p5_regex_capture = [];
    var res = [];
    var myArray;
    var last_index = 0;
    while ((myArray = re.exec(s)) !== null) {
        var m = myArray.shift();
        p5_regex_capture = [].concat(myArray);
        if (myArray.index > last_index) {
            res.push(s.substr(last_index, myArray.index - last_index));
        }
        res.push(fun_replace());
        last_index = re.lastIndex;
        if (last_index == 0) {
            count = 1;
            last_index = myArray.index + m.length;
            if (s.length > last_index) {
                res.push(s.substr(last_index, s.length - last_index));
            }
            return [res.join(''), count]
        }
        count++;
    }
    if (s.length > last_index) {
        res.push(s.substr(last_index, s.length - last_index));
    }
    return [res.join(''), count]
};

var p5tr = function(s, search, replace, modifier, want) {
    var count = 0;
    // TODO - expand character lists in spec
    // TODO - modifiers
    search = search.split("");
    replace = replace.split("");
    while (search.length > replace.length) {
        replace.push(replace[replace.length-1]);
    }
    var tr = {};
    for(var i = 0; i < search.length; i++) {
        tr[search[i]] = replace[i];
    }
    var res = s.split("");
    for(var i = 0; i < res.length; i++) {
        if (tr.hasOwnProperty(res[i])) {
            res[i] = tr[res[i]];
            count++;
        }
    }
    return [res.join(''), count]
};

var p5chop = function(s) {
    // TODO - hash

    if (s instanceof Array) {
        // perl5 array
        var count = 0;
        var res;
        for (var j = 0; j < s.length; j++) {
            res = p5chop(p5str(s[j]));
            count = res[0];
            s[j] = res[1];
        }
        return [count, s];
    }

    s = p5str(s);
    return [s.substr(-1,1), s.substr(0,s.length-1)]
};

var p5chomp = function(s) {
    // TODO - hash
    // TODO - special cases of $/ - empty string, reference

    if (s instanceof Array) {
        // perl5 array
        var count = 0;
        var res;
        for (var j = 0; j < s.length; j++) {
            res = p5chomp(p5str(s[j]));
            count = count + res[0];
            s[j] = res[1];
        }
        return [count, s];
    }

    s = p5str(s);
    var sep = p5pkg["main"]["v_/"];  // $/
    var c = s.substr(-sep.length);
    if (c == sep) {
        return [c.length, s.substr(0,s.length-sep.length)]
    }
    else {
        return [0, s]
    }
};

var p5vec_set = function(sb, offset, bits, value) {
    if (offset < 0) {
        return CORE.die(["Negative offset to vec in lvalue context: " + offset]);
    }
    sb = p5str(sb);
    if (bits == 1) {
        var byteOfs = Math.floor(offset / 8);
        var bitOfs  = offset - 8 * byteOfs;
        var mask = 1;
        value = (value & mask) << bitOfs;
        mask = mask << bitOfs;
        if (byteOfs < sb.length) {
            value = (sb.charCodeAt(byteOfs) & ~mask) | value;
        }
        offset = byteOfs;
        bits = 8;
    }
    if (bits == 2) {
        var byteOfs = Math.floor(offset / 4);
        var bitOfs  = 2 * (offset - 4 * byteOfs);
        var mask = 3;
        value = (value & mask) << bitOfs;
        mask = mask << bitOfs;
        if (byteOfs < sb.length) {
            value = (sb.charCodeAt(byteOfs) & ~mask) | value;
        }
        offset = byteOfs;
        bits = 8;
    }
    if (bits == 4) {
        var byteOfs = Math.floor(offset / 2);
        var bitOfs  = 4 * (offset - 2 * byteOfs);
        var mask = 15;
        value = (value & mask) << bitOfs;
        mask = mask << bitOfs;
        if (byteOfs < sb.length) {
            value = (sb.charCodeAt(byteOfs) & ~mask) | value;
        }
        offset = byteOfs;
        bits = 8;
    }
    if (bits == 8) {
        if (offset == 0) {
            if (sb.length < 2) {
                sb = String.fromCharCode(value);
            }
            else {
                sb = String.fromCharCode(value) + sb.substr(1);
            }
        }
        else {
            while (offset >= sb.length) {
                sb = sb + String.fromCharCode(0);
            }
            if (sb.length <= offset) {
                sb = sb.substr(0, offset) + String.fromCharCode(value);
            }
            else {
                sb = sb.substr(0, offset) + String.fromCharCode(value) + sb.substr(offset + 1);
            }
        }
        return sb;
    }
    if (bits == 16) {
        sb = p5vec_set(sb, offset,     8, (value >> 8) & 256);
        sb = p5vec_set(sb, offset + 1, 8, value & 256);
        return sb;
    }
    if (bits == 32) {
        sb = p5vec_set(sb, offset,     8, (value >> 24) & 256);
        sb = p5vec_set(sb, offset + 1, 8, (value >> 16) & 256);
        sb = p5vec_set(sb, offset + 2, 8, (value >> 8)  & 256);
        sb = p5vec_set(sb, offset + 3, 8, value & 256);
        return sb;
    }
    return CORE.die(["Illegal number of bits in vec: " + bits]);
}

var p5for = function(namespace, var_name, func, args, cont, label) {
    var local_idx = p5LOCAL.length;
    var v_old = namespace[var_name];
    var _redo;
    p5LOCAL.push(function(){ namespace[var_name] = v_old });
    for(var i = 0; i < args.length; i++) {
        namespace[var_name] = args[i];
        do {
            _redo = false;
            try {
                func()
            }
            catch(err) {
                if (err instanceof p5_error && (err.v == label || err.v == '')) {
                    if (err.type == 'last') {
                        p5cleanup_local(local_idx, null);
                        return
                    }
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
                    if (err instanceof p5_error && (err.v == label || err.v == '')) {
                        if (err.type == 'last') {
                            p5cleanup_local(local_idx, null);
                            return
                        }
                        else if (err.type == 'redo') { _redo = true }
                        else if (err.type != 'next') { throw(err) }
                    }            
                    else {
                        throw(err)
                    }
                }
            }
        } while (_redo);
    }
    p5cleanup_local(local_idx, null);
};

var p5for_lex = function(set_var, func, args, cont, label) {
    var local_idx = p5LOCAL.length;
    var _redo;
    for(var i = 0; i < args.length; i++) {
        set_var(args[i]);
        do {
            _redo = false;
            try {
                func()
            }
            catch(err) {
                if (err instanceof p5_error && (err.v == label || err.v == '')) {
                    if (err.type == 'last') {
                        p5cleanup_local(local_idx, null);
                        return
                    }
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
                    if (err instanceof p5_error && (err.v == label || err.v == '')) {
                        if (err.type == 'last') {
                            p5cleanup_local(local_idx, null);
                            return
                        }
                        else if (err.type == 'redo') { _redo = true }
                        else if (err.type != 'next') { throw(err) }
                    }            
                    else {
                        throw(err)
                    }
                }
            }
        } while (_redo);
    }
    p5cleanup_local(local_idx, null);
};

var p5block = function(set_var, func, args, cont, label) {
    var local_idx = p5LOCAL.length;
    var _redo;
    for(var i = 0; i < args.length; i++) {
        set_var(args[i]);
        do {
            _redo = false;
            try {
                return func()
            }
            catch(err) {
                if (err instanceof p5_error && (err.v == label || err.v == '')) {
                    if (err.type == 'last') {
                        p5cleanup_local(local_idx, null);
                        return
                    }
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
                    if (err instanceof p5_error && (err.v == label || err.v == '')) {
                        if (err.type == 'last') {
                            p5cleanup_local(local_idx, null);
                            return
                        }
                        else if (err.type == 'redo') { _redo = true }
                        else if (err.type != 'next') { throw(err) }
                    }            
                    else {
                        throw(err)
                    }
                }
            }
        } while (_redo);
    }
    p5cleanup_local(local_idx, null);
};


var p5while = function(func, cond, cont, label, redo) {
    var local_idx = p5LOCAL.length;
    while (redo || p5bool(cond())) {
        redo = false;
        try {
            func()
        }
        catch(err) {
            if (err instanceof p5_error && (err.v == label || err.v == '')) {
                if (err.type == 'last') {
                    p5cleanup_local(local_idx, null);
                    return
                }
                else if (err.type == 'redo') { redo = true }
                else if (err.type != 'next') { throw(err) }
            }            
            else {
                throw(err)
            }
        }
        if (cont) {
            try {
                if (!redo) { cont() }
            }
            catch(err) {
                if (err instanceof p5_error && (err.v == label || err.v == '')) {
                    if (err.type == 'last') {
                        p5cleanup_local(local_idx, null);
                        return
                    }
                    else if (err.type == 'redo') { redo = true }
                    else if (err.type != 'next') { throw(err) }
                }            
                else {
                    throw(err)
                }
            }
        }
    }
    p5cleanup_local(local_idx, null);
};

var p5map = function(namespace, func, args) {
    var v_old = p5pkg["main"]["v__"];
    var out = [];
    for(var i = 0; i < args.length; i++) {
        p5pkg["main"]["v__"] = args[i];
        var o = p5list_to_a([func(1)]);
        for(var j = 0; j < o.length; j++) {
            out.push(o[j]);
        }
    }
    p5pkg["main"]["v__"] = v_old;
    return out;
};

var p5grep = function(namespace, func, args) {
    var v_old = p5pkg["main"]["v__"];
    var out = [];
    for(var i = 0; i < args.length; i++) {
        p5pkg["main"]["v__"] = args[i];
        if (p5bool(func(0))) {
            out.push(args[i])
        }
    }
    p5pkg["main"]["v__"] = v_old;
    return out;
};

var p5sort = function(namespace, func, args) {
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

function p5hash_delete(v,k) {
    var res = v[k];
    delete (v[k]);
    return res;
}
function p5hash_delete_list(v,k) {
    var res = [];
    for (var i = 0; i < k.length; i++) {
        res.push(v[k[i]]);
        delete (v[k[i]]);
    }
    return res;
}
function p5deleteSymbolTable(v,k) {
    if (!k instanceof Array) {
        k = [k];
    }
    for (var i = 0; i < k.length; i++) {
        delete (p5pkg[v][k[i]]);
        delete (p5pkg[v]["v_" + k[i]]);
        delete (p5pkg[v]["List_" + k[i]]);
        delete (p5pkg[v]["Hash_" + k[i]]);
    }
}

EOT

} # end of emit_javascript2()

1;


