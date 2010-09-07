// include file: lib/Perlito/Javascript/Runtime.js
// lib/Perlito/Javascript/Runtime.js
//
// Runtime for "Perlito" Perlito-in-Javascript
//
// AUTHORS
//
// Flavio Soibelmann Glock  fglock@gmail.com
// The Pugs Team  perl6-compiler@perl.org
//
// SEE ALSO
//
// The Perl 6 homepage at http://dev.perl.org/perl6
// The Pugs homepage at http://pugscode.org/
//
// COPYRIGHT
//
// Copyright 2009, 2010 by Flavio Soibelmann Glock and others.
// 
// This program is free software; you can redistribute it and/or modify it
// under the same terms as Perl itself.
// 
// See http://www.perl.com/perl/misc/Artistic.html

List_ARGS = [];
if (typeof arguments == 'object' ) {
    List_ARGS = arguments;
}

// class IO
if (typeof IO != 'object') {
  IO = function() {};
  IO = new IO;
}
IO.f_slurp = function (filename) {
    if (typeof readFile == 'function' ) {
        return readFile(filename);
    }
    f_die("IO.slurp() not implemented");    
}

// class Main
if (typeof Main != 'object') {
  Main = function() {};
  Main = new Main;
}
(function () {
  // method newline
  Main.f_newline = function () {
    return "\n";
  }
  Main.f_lisp_escape_string = function (s) {
    var o = s;
    o = o.replace( /\\/g, "\\\\");
    o = o.replace( /"/g, "\\\"");
    return o;
  }
  Main.f_javascript_escape_string = function (s) {
    var o = s;
    o = o.replace( /\\/g, "\\\\");
    o = o.replace( /"/g, "\\\"");
    o = o.replace( /\n/g, "\\n");
    return o;
  }
  Main.f_perl_escape_string = function (s) {
    var o = s;
    o = o.replace( /\\/g, "\\\\");
    o = o.replace( /'/g, "\\'");
    return o;
  }
  Main.f_to_javascript_namespace = function (s) {
    var o = s;
    o = o.replace( /::/g, "$");
    return o;
  }
  Main.f_to_lisp_namespace = function (s) {
    var o = s;
    o = o.replace( /[$@%]/, "");
    o = o.replace( /::/g, "-");
    return "mp-" + o;
  }
  Main.f_to_go_namespace = function (s) {
    var o = s;
    o = o.replace( /[$@%]/, "");
    o = o.replace( /::/g, "__");
    return o;
  }
  Main._dump = function (o) {
    var out = [];
    for(var i in o) { 
      if ( i.match( /^v_/ ) ) {
        out.push( i.substr(2) + " => " + f_perl(o[i]) ) 
      }
    }
    return out.join(", ");
  }
})();

if (typeof Perlito$Match != 'object') {
  Perlito$Match = function() {};
  Perlito$Match = new Perlito$Match;
  Perlito$Match.f_isa = function (s) { return s == 'Perlito::Match' };
  Perlito$Match.f_perl = function () { return 'Perlito::Match.new(' + Main._dump(this) + ')' };
}
v_MATCH = {};
v_MATCH.__proto__ = Perlito$Match;
Perlito$Match.f_hash = function () { return this }

if (typeof f_print != 'function') {
    var buf = "";
    f_print = function () { 
        var i;
        for (i = 0; i < f_print.arguments.length; i++) {
            var s = f_string( f_print.arguments[i] ) 
            if ( s.substr(s.length - 2, 2 ) == "\n" ) {
                print( buf + s.substr(0, s.length - 2) );
                buf = "";
            }
            else if ( s.substr(s.length - 1, 1 ) == "\n" ) {
                print( buf + s.substr(0, s.length - 1) );
                buf = "";
            }
            else {
                buf = buf + s;
            }
        }
    }
}
if (typeof f_say != 'function') {
  f_say = function () {
    var i;
    for (i = 0; i < f_say.arguments.length; i++) {
        f_print( f_say.arguments[i] );
    }
    f_print("\n");
  }
}
if (typeof f_die != 'function') {
  f_die = function (s) {
    print("Died: " + s + "\n")
  }
}
if (typeof f_warn != 'function') {
  f_warn = function (s) {
    print("Warning: " + s + "\n")
  }
}
f_chr = function (o) {
  return String.fromCharCode(o)
}
f_elems = function (o) {
  if ( o == null ) { return 1 };
  if ( typeof o.f_elems == 'function' ) { return o.f_elems() }
  if ( typeof o == 'object' && (o instanceof Array) ) {
    return o.length;
  }
  switch (typeof o){
    case "string":   return 1;
    case "function": return 1; 
    case "number":   return 1;
    case "boolean":  return 1;
  }
  var l = 0;
  for(var i in o) { 
    l++ 
  }
  return l;
} 
f_values = function (o) {
  if ( o == null ) { return [] };
  if ( typeof o.f_values == 'function' ) { return o.f_values() }
  if ( typeof o == 'object' && (o instanceof Array) ) {
    return o;
  }
  switch (typeof o){
    case "string":   return [o];
    case "function": return [o]; 
    case "number":   return [o];
    case "boolean":  return [o];
  }
  var out = [];
  for(var i in o) { 
    out.push( o[i] ) 
  }
  return out;
}
f_keys = function (o) {
  if ( o == null ) { return [] };
  if ( typeof o.f_keys == 'function' ) { return o.f_keys() }
  var out = [];
  if ( typeof o == 'object' && (o instanceof Array) ) {
    var count = 0;
    for(var i in o) { 
      out.push(count) 
      count++;
    }
    return o;
  }
  for(var i in o) { 
    out.push(i) 
  }
  return out;
}
f_perl = function (o) {
  if ( o == null ) { return 'Mu' };
  if ( typeof o.f_perl == 'function' ) { return o.f_perl() }
  if ( typeof o == 'object' && (o instanceof Array) ) {
    var out = [];
    for(var i = 0; i < o.length; i++) { out.push( f_perl(o[i]) ) }
    return "[" + out.join(", ") + "]";
  }
  switch (typeof o){
    case "string":   return '"' + Main.f_lisp_escape_string(o) + '"';
    case "function": return "function"; 
    case "number":   return o;
    case "boolean":  return o;
  }
    var out = [];
    for(var i in o) { 
      out.push( i + " => " + f_perl(o[i]) ) 
    }
    return '{' + out.join(", ") + '}';
}
f_isa = function (o, s) {
  if ( o == null ) { 
    if ( s == 'Mu' ) { return true } else { return false }
  }
  if ( typeof o.f_isa == 'function' ) { return o.f_isa(s) }
  switch (typeof o){
    case "string":   return(s == 'Str');
    case "number":   return(s == 'Num');
  }
  if ( s == 'Array' && typeof o == 'object' && (o instanceof Array) ) { return(1) }
  return false;
}
f_scalar = function (o) { 
  if ( o == null ) { return o }
  if ( typeof o.f_scalar == 'function' ) { return o.f_scalar() }
  return o;
}
f_string = function (o) { 
  if ( o == null ) { return "" }
  if ( typeof o == 'object' && (o instanceof Array) ) {
    var out = [];
    for(var i = 0; i < o.length; i++) { out.push( f_string(o[i]) ) }
    return out.join(" ");
  }
  if ( typeof o.f_string == 'function' ) { return o.f_string() }
  if ( typeof o != 'string' ) { return "" + o }
  return o;
}
f_add = function (o1, o2) { 
  if ( typeof o1 == 'string' ) { 
    if ( typeof o2 == 'string' ) { 
      return parseFloat(o1) + parseFloat(o2)
    }
    return parseFloat(o1) + o2 
  }
  if ( typeof o2 == 'string' ) { 
    return o1 + parseFloat(o2)
  }
  return o1 + o2;
}
f_bool = function (o) {
  if ( o == null ) { return o }
  if ( typeof o == 'boolean' ) { return o }
  if ( typeof o == 'number'  ) { return o }
  if ( typeof o == 'string'  ) { return o != '' && o != '0' }
  if ( typeof o.f_bool == 'function' ) { return o.v_bool }
  if ( typeof o.length == 'number' ) { return o.length }
  return o;
}
f_and = function (a, fb) {
  if (f_bool(a)) { return fb() }
  return a
}
f_or = function (a, fb) {
  if (f_bool(a)) { return a }
  return fb()
}
f_defined_or = function (a, fb) {
  if ( a == null ) { return fb() }
  return a
}
f_pop = function (o) {
  if (o.length == null ) { return null }
  return o.pop();
}
f_shift = function (o) {
  if (o.length == null ) { return null }
  return o.shift();
}
f_push = function (o, v) {
  return o.push(v);
}
f_unshift = function (o, v) {
  return o.unshift(v);
}
f_index = function (o, s) {
  return o.indexOf(s);
}
f_chars = function (o) { 
  if ( typeof o.f_string == 'function' ) { return o.f_string().length }
  return o.length;
}

// regex primitives
if (typeof Perlito$Grammar != 'object') {
  Perlito$Grammar = function() {};
  Perlito$Grammar = new Perlito$Grammar;
}
Perlito$Grammar.f_word = function (v_str, v_pos) { 
    var tmp = {           
            v_str:  v_str,
            v_from: v_pos, 
            v_to:   v_pos + 1,
            v_bool: v_str.substr(v_pos, 1).match(/\w/) != null
        };
    tmp.__proto__ = Perlito$Match;
    return tmp;
} 
Perlito$Grammar.f_digit = function (v_str, v_pos) { 
    var tmp = {           
            v_str:  v_str,
            v_from: v_pos, 
            v_to:   v_pos + 1,
            v_bool: v_str.substr(v_pos, 1).match(/\d/) != null
        };
    tmp.__proto__ = Perlito$Match;
    return tmp;
} 
Perlito$Grammar.f_space = function (v_str, v_pos) { 
    var tmp = {           
            v_str:  v_str,
            v_from: v_pos, 
            v_to:   v_pos + 1,
            v_bool: v_str.substr(v_pos, 1).match(/\s/) != null
        };
    tmp.__proto__ = Perlito$Match;
    return tmp;
} 
Perlito$Grammar.f_is_newline = function (v_str, v_pos) { 
    var m_ = v_str.substr(v_pos).match(/^(\r\n?|\n\r?)/);
    var tmp = {           
            v_str:  v_str,
            v_from: v_pos, 
            v_to:   m_ != null ? v_pos + m_[0].length : v_pos,
            v_bool: m_ != null
        };
    tmp.__proto__ = Perlito$Match;
    return tmp;
} 
Perlito$Grammar.f_not_newline = function (v_str, v_pos) { 
    var tmp = {           
            v_str:  v_str,
            v_from: v_pos, 
            v_to:   v_pos + 1,
            v_bool: v_str.substr(v_pos, 1).match(/[^\r\n]/) != null
        };
    tmp.__proto__ = Perlito$Match;
    return tmp;
} 


// end include file: lib/Perlito/Javascript/Runtime.js
// Do not edit this file - Generated by Perlito 6.0
// class GLOBAL
if (typeof GLOBAL != 'object') {
  GLOBAL = function() {};
  GLOBAL = new GLOBAL;
  GLOBAL.f_isa = function (s) { return s == 'GLOBAL' };
  GLOBAL.f_perl = function () { return 'GLOBAL.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = GLOBAL;
// use v6
;// class Perlito::Match
if (typeof Perlito$Match != 'object') {
  Perlito$Match = function() {};
  Perlito$Match = new Perlito$Match;
  Perlito$Match.f_isa = function (s) { return s == 'Perlito::Match' };
  Perlito$Match.f_perl = function () { return 'Perlito::Match.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Perlito$Match;
  // accessor from
  Perlito$Match.v_from = null;
  Perlito$Match.f_from = function () { return this.v_from }
  // accessor to
  Perlito$Match.v_to = null;
  Perlito$Match.f_to = function () { return this.v_to }
  // accessor str
  Perlito$Match.v_str = null;
  Perlito$Match.f_str = function () { return this.v_str }
  // accessor bool
  Perlito$Match.v_bool = null;
  Perlito$Match.f_bool = function () { return this.v_bool }
  // accessor capture
  Perlito$Match.v_capture = null;
  Perlito$Match.f_capture = function () { return this.v_capture }
  // method scalar
  Perlito$Match.f_scalar = function () {
    var v_self = this;
    try { if ( f_bool(v_self.v_bool) ) { return (function () { if ( f_bool((v_self.v_capture != null)) ) { (function () { throw(v_self.v_capture); })() };throw((v_self.v_str || "").substr(v_self.v_from, ((v_self.v_to - v_self.v_from)))) })() } else { return (function () { throw("") })() } } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Match.f_scalar;  // v8 bug workaround
  // method string
  Perlito$Match.f_string = function () {
    var v_self = this;
    try { if ( f_bool(v_self.v_bool) ) { return (function () { if ( f_bool((v_self.v_capture != null)) ) { (function () { throw(v_self.v_capture); })() };throw((v_self.v_str || "").substr(v_self.v_from, ((v_self.v_to - v_self.v_from)))) })() } else { return (function () { throw("") })() } } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Match.f_string;  // v8 bug workaround
})();
;// class Pair
if (typeof Pair != 'object') {
  Pair = function() {};
  Pair = new Pair;
  Pair.f_isa = function (s) { return s == 'Pair' };
  Pair.f_perl = function () { return 'Pair.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Pair;
  // accessor key
  Pair.v_key = null;
  Pair.f_key = function () { return this.v_key }
  // accessor value
  Pair.v_value = null;
  Pair.f_value = function () { return this.v_value }
  // method perl
  Pair.f_perl = function () {
    var v_self = this;
    try { throw((f_string(v_self.v_key) + f_string(" => ") + f_string(f_perl(v_self.v_value)))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Pair.f_perl;  // v8 bug workaround
})();
;// class Main
if (typeof Main != 'object') {
  Main = function() {};
  Main = new Main;
  Main.f_isa = function (s) { return s == 'Main' };
  Main.f_perl = function () { return 'Main.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Main;
  // sub to_lisp_identifier
  Main.f_to_lisp_identifier = function (v_ident) {
    try { throw((f_string("sv-") + f_string(v_ident))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  // sub lisp_dump_object
  Main.f_lisp_dump_object = function (v_class_name, v_data) {
    try { throw((f_string(v_class_name) + f_string("( ") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( f_perl(a_[i]) ) } return out; })(v_data)).join(", ")) + f_string(" )"))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
})();
;})();

// class GLOBAL
if (typeof GLOBAL != 'object') {
  GLOBAL = function() {};
  GLOBAL = new GLOBAL;
  GLOBAL.f_isa = function (s) { return s == 'GLOBAL' };
  GLOBAL.f_perl = function () { return 'GLOBAL.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = GLOBAL;
// use v6
;// class Perlito::Javascript::LexicalBlock
if (typeof Perlito$Javascript$LexicalBlock != 'object') {
  Perlito$Javascript$LexicalBlock = function() {};
  Perlito$Javascript$LexicalBlock = new Perlito$Javascript$LexicalBlock;
  Perlito$Javascript$LexicalBlock.f_isa = function (s) { return s == 'Perlito::Javascript::LexicalBlock' };
  Perlito$Javascript$LexicalBlock.f_perl = function () { return 'Perlito::Javascript::LexicalBlock.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Perlito$Javascript$LexicalBlock;
  // accessor block
  Perlito$Javascript$LexicalBlock.v_block = null;
  Perlito$Javascript$LexicalBlock.f_block = function () { return this.v_block }
  // accessor needs_return
  Perlito$Javascript$LexicalBlock.v_needs_return = null;
  Perlito$Javascript$LexicalBlock.f_needs_return = function () { return this.v_needs_return }
  // accessor top_level
  Perlito$Javascript$LexicalBlock.v_top_level = null;
  Perlito$Javascript$LexicalBlock.f_top_level = function () { return this.v_top_level }
  // method emit_javascript
  Perlito$Javascript$LexicalBlock.f_emit_javascript = function () {
    var v_self = this;
    try { var v_str = null;
var v_last_statement = null;
if ( f_bool(( f_bool(v_self.v_block) ? false : true)) ) { (function () { throw("null"); })() };(v_str = "");(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { (function (v_decl) { if ( f_bool(f_and(f_isa(v_decl, "Decl"), function () { return (v_decl.f_decl() == "my")})) ) { (function () { (v_str = (f_string(v_str) + f_string(v_decl.f_emit_javascript_init()))); })() };if ( f_bool(f_and(f_isa(v_decl, "Apply"), function () { return (v_decl.f_code() == "infix:<=>")})) ) { (function () { var v_var = null;
(v_var = v_decl.f_arguments()[0]);if ( f_bool(f_and(f_isa(v_var, "Decl"), function () { return (v_var.f_decl() == "my")})) ) { (function () { (v_str = (f_string(v_str) + f_string(v_var.f_emit_javascript_init()))); })() }; })() }; })(a_[i_]) } })(v_self.v_block);if ( f_bool(v_self.v_needs_return) ) { (function () { (v_last_statement = f_pop(v_self.v_block)); })() };(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { (function (v_decl) { if ( f_bool(( f_bool((f_and(f_isa(v_decl, "Decl"), function () { return (v_decl.f_decl() == "my")}))) ? false : true)) ) { (function () { (v_str = (f_string(v_str) + f_string(v_decl.f_emit_javascript()) + f_string(";"))); })() }; })(a_[i_]) } })(v_self.v_block);if ( f_bool(f_and(v_self.v_needs_return, function () { return v_last_statement})) ) { (function () { if ( f_bool(f_isa(v_last_statement, "If")) ) { (function () { var v_cond = null;
var v_body = null;
var v_otherwise = null;
(v_cond = v_last_statement.f_cond());(v_body = v_last_statement.f_body());(v_otherwise = v_last_statement.f_otherwise());if ( f_bool(f_and(f_isa(v_cond, "Var"), function () { return (v_cond.f_sigil() == "@")})) ) { (function () { (v_cond = (function () { var tmp = {v_code: "prefix:<@>",v_arguments: [v_cond]}; tmp.__proto__ = Apply; return tmp })()); })() };(v_body = (function () { var tmp = {v_block: v_body.f_stmts(),v_needs_return: 1}; tmp.__proto__ = Perlito$Javascript$LexicalBlock; return tmp })());(v_str = (f_string(v_str) + f_string("if ( f_bool(") + f_string(v_cond.f_emit_javascript()) + f_string(") ) { ") + f_string("return (function () { ") + f_string(v_body.f_emit_javascript()) + f_string(" })() }")));if ( f_bool(v_otherwise) ) { (function () { (v_otherwise = (function () { var tmp = {v_block: v_otherwise.f_stmts(),v_needs_return: 1}; tmp.__proto__ = Perlito$Javascript$LexicalBlock; return tmp })());(v_str = (f_string(v_str) + f_string(" else { ") + f_string("return (function () { ") + f_string(v_otherwise.f_emit_javascript()) + f_string(" })() }"))); })() }; })() } else { (function () { if ( f_bool(f_or(f_and(f_isa(v_last_statement, "Apply"), function () { return (v_last_statement.f_code() == "return")}), function () { return f_isa(v_last_statement, "For")})) ) { (function () { (v_str = (f_string(v_str) + f_string(v_last_statement.f_emit_javascript()))); })() } else { (function () { (v_str = (f_string(v_str) + f_string("return(") + f_string(v_last_statement.f_emit_javascript()) + f_string(")"))); })() }; })() }; })() };if ( f_bool(v_self.v_top_level) ) { (function () { (v_str = (f_string("try { ") + f_string(v_str) + f_string(" } catch(err) { ") + f_string("if ( err instanceof Error ) { ") + f_string("throw(err) ") + f_string("} ") + f_string("else { ") + f_string("return(err) ") + f_string("} ") + f_string("} "))); })() };throw(v_str) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Javascript$LexicalBlock.f_emit_javascript;  // v8 bug workaround
})();
;// class CompUnit
if (typeof CompUnit != 'object') {
  CompUnit = function() {};
  CompUnit = new CompUnit;
  CompUnit.f_isa = function (s) { return s == 'CompUnit' };
  CompUnit.f_perl = function () { return 'CompUnit.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = CompUnit;
  // accessor name
  CompUnit.v_name = null;
  CompUnit.f_name = function () { return this.v_name }
  // accessor attributes
  CompUnit.v_attributes = null;
  CompUnit.f_attributes = function () { return this.v_attributes }
  // accessor methods
  CompUnit.v_methods = null;
  CompUnit.f_methods = function () { return this.v_methods }
  // accessor body
  CompUnit.v_body = null;
  CompUnit.f_body = function () { return this.v_body }
  // method emit_javascript
  CompUnit.f_emit_javascript = function () {
    var v_self = this;
    try { var v_class_name = null;
var v_str = null;
(v_class_name = Main.f_to_javascript_namespace(v_self.v_name));(v_str = (f_string("// class ") + f_string(v_self.v_name) + f_string((f_string("\n"))) + f_string("if (typeof ") + f_string(v_class_name) + f_string(" != 'object') {") + f_string((f_string("\n"))) + f_string("  ") + f_string(v_class_name) + f_string(" = function() {};") + f_string((f_string("\n"))) + f_string("  ") + f_string(v_class_name) + f_string(" = new ") + f_string(v_class_name) + f_string(";") + f_string((f_string("\n"))) + f_string("  ") + f_string(v_class_name) + f_string(".f_isa = function (s) { return s == '") + f_string(v_self.v_name) + f_string("' };") + f_string((f_string("\n"))) + f_string("  ") + f_string(v_class_name) + f_string(".f_perl = function () { return '") + f_string(v_self.v_name) + f_string(".new(' + Main._dump(this) + ')' };") + f_string((f_string("\n"))) + f_string("}") + f_string((f_string("\n"))) + f_string("(function () {") + f_string((f_string("\n"))) + f_string("  var v__NAMESPACE = ") + f_string(v_class_name) + f_string(";") + f_string((f_string("\n")))));(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { (function (v_decl) { if ( f_bool(f_and(f_isa(v_decl, "Decl"), function () { return ((v_decl.f_decl() == "my"))})) ) { (function () { (v_str = (f_string(v_str) + f_string(v_decl.f_emit_javascript_init()))); })() };if ( f_bool(f_and(f_isa(v_decl, "Apply"), function () { return (v_decl.f_code() == "infix:<=>")})) ) { (function () { var v_var = null;
(v_var = v_decl.f_arguments()[0]);if ( f_bool(f_and(f_isa(v_var, "Decl"), function () { return (v_var.f_decl() == "my")})) ) { (function () { (v_str = (f_string(v_str) + f_string(v_var.f_emit_javascript_init()))); })() }; })() }; })(a_[i_]) } })(v_self.v_body);(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { (function (v_decl) { if ( f_bool(f_and(f_isa(v_decl, "Decl"), function () { return ((v_decl.f_decl() == "has"))})) ) { (function () { (v_str = (f_string(v_str) + f_string("  // accessor ") + f_string(v_decl.f_var().f_name()) + f_string((f_string("\n"))) + f_string("  ") + f_string(v_class_name) + f_string(".v_") + f_string(v_decl.f_var().f_name()) + f_string(" = null;") + f_string((f_string("\n"))) + f_string("  ") + f_string(v_class_name) + f_string(".f_") + f_string(v_decl.f_var().f_name()) + f_string(" = function () { return this.v_") + f_string(v_decl.f_var().f_name()) + f_string(" }") + f_string((f_string("\n"))))); })() };if ( f_bool(f_isa(v_decl, "Method")) ) { (function () { var v_sig = null;
var v_pos = null;
var v_invocant = null;
var v_block = null;
(v_sig = v_decl.f_sig());(v_pos = v_sig.f_positional());(v_invocant = v_sig.f_invocant());(v_block = (function () { var tmp = {v_block: v_decl.f_block(),v_needs_return: 1,v_top_level: 1}; tmp.__proto__ = Perlito$Javascript$LexicalBlock; return tmp })());(v_str = (f_string(v_str) + f_string("  // method ") + f_string(v_decl.f_name()) + f_string((f_string("\n"))) + f_string("  ") + f_string(v_class_name) + f_string(".f_") + f_string(v_decl.f_name()) + f_string(" = function (") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_pos)).join(", ")) + f_string(") {") + f_string((f_string("\n"))) + f_string("    var ") + f_string(v_invocant.f_emit_javascript()) + f_string(" = this;") + f_string((f_string("\n"))) + f_string("    ") + f_string(v_block.f_emit_javascript()) + f_string((f_string("\n"))) + f_string("  }") + f_string((f_string("\n"))) + f_string("  ") + f_string(v_class_name) + f_string(".f_") + f_string(v_decl.f_name()) + f_string(";  // v8 bug workaround") + f_string((f_string("\n"))))); })() };if ( f_bool(f_isa(v_decl, "Sub")) ) { (function () { var v_sig = null;
var v_pos = null;
var v_block = null;
(v_sig = v_decl.f_sig());(v_pos = v_sig.f_positional());(v_block = (function () { var tmp = {v_block: v_decl.f_block(),v_needs_return: 1,v_top_level: 1}; tmp.__proto__ = Perlito$Javascript$LexicalBlock; return tmp })());(v_str = (f_string(v_str) + f_string("  // sub ") + f_string(v_decl.f_name()) + f_string((f_string("\n"))) + f_string("  ") + f_string(v_class_name) + f_string(".f_") + f_string(v_decl.f_name()) + f_string(" = function (") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_pos)).join(", ")) + f_string(") {") + f_string((f_string("\n"))) + f_string("    ") + f_string(v_block.f_emit_javascript()) + f_string((f_string("\n"))) + f_string("  }") + f_string((f_string("\n"))))); })() }; })(a_[i_]) } })(v_self.v_body);(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { (function (v_decl) { if ( f_bool(f_and(f_and((( f_bool((f_and(f_isa(v_decl, "Decl"), function () { return (f_or(((v_decl.f_decl() == "has")), function () { return ((v_decl.f_decl() == "my"))}))}))) ? false : true)), function () { return (( f_bool((f_isa(v_decl, "Method"))) ? false : true))}), function () { return (( f_bool((f_isa(v_decl, "Sub"))) ? false : true))})) ) { (function () { (v_str = (f_string(v_str) + f_string((v_decl).f_emit_javascript()) + f_string(";"))); })() }; })(a_[i_]) } })(v_self.v_body);return((v_str = (f_string(v_str) + f_string("}") + f_string(")();") + f_string((f_string("\n")))))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  CompUnit.f_emit_javascript;  // v8 bug workaround
})();
;// class Val::Int
if (typeof Val$Int != 'object') {
  Val$Int = function() {};
  Val$Int = new Val$Int;
  Val$Int.f_isa = function (s) { return s == 'Val::Int' };
  Val$Int.f_perl = function () { return 'Val::Int.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Val$Int;
  // accessor int
  Val$Int.v_int = null;
  Val$Int.f_int = function () { return this.v_int }
  // method emit_javascript
  Val$Int.f_emit_javascript = function () {
    var v_self = this;
    try { return(v_self.v_int) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Val$Int.f_emit_javascript;  // v8 bug workaround
})();
;// class Val::Bit
if (typeof Val$Bit != 'object') {
  Val$Bit = function() {};
  Val$Bit = new Val$Bit;
  Val$Bit.f_isa = function (s) { return s == 'Val::Bit' };
  Val$Bit.f_perl = function () { return 'Val::Bit.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Val$Bit;
  // accessor bit
  Val$Bit.v_bit = null;
  Val$Bit.f_bit = function () { return this.v_bit }
  // method emit_javascript
  Val$Bit.f_emit_javascript = function () {
    var v_self = this;
    try { return(( f_bool(v_self.v_bit) ? "true" : "false")) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Val$Bit.f_emit_javascript;  // v8 bug workaround
})();
;// class Val::Num
if (typeof Val$Num != 'object') {
  Val$Num = function() {};
  Val$Num = new Val$Num;
  Val$Num.f_isa = function (s) { return s == 'Val::Num' };
  Val$Num.f_perl = function () { return 'Val::Num.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Val$Num;
  // accessor num
  Val$Num.v_num = null;
  Val$Num.f_num = function () { return this.v_num }
  // method emit_javascript
  Val$Num.f_emit_javascript = function () {
    var v_self = this;
    try { return(v_self.v_num) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Val$Num.f_emit_javascript;  // v8 bug workaround
})();
;// class Val::Buf
if (typeof Val$Buf != 'object') {
  Val$Buf = function() {};
  Val$Buf = new Val$Buf;
  Val$Buf.f_isa = function (s) { return s == 'Val::Buf' };
  Val$Buf.f_perl = function () { return 'Val::Buf.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Val$Buf;
  // accessor buf
  Val$Buf.v_buf = null;
  Val$Buf.f_buf = function () { return this.v_buf }
  // method emit_javascript
  Val$Buf.f_emit_javascript = function () {
    var v_self = this;
    try { return((f_string("\"") + f_string(Main.f_javascript_escape_string(v_self.v_buf)) + f_string("\""))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Val$Buf.f_emit_javascript;  // v8 bug workaround
})();
;// class Lit::Block
if (typeof Lit$Block != 'object') {
  Lit$Block = function() {};
  Lit$Block = new Lit$Block;
  Lit$Block.f_isa = function (s) { return s == 'Lit::Block' };
  Lit$Block.f_perl = function () { return 'Lit::Block.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Lit$Block;
  // accessor sig
  Lit$Block.v_sig = null;
  Lit$Block.f_sig = function () { return this.v_sig }
  // accessor stmts
  Lit$Block.v_stmts = null;
  Lit$Block.f_stmts = function () { return this.v_stmts }
  // method emit_javascript
  Lit$Block.f_emit_javascript = function () {
    var v_self = this;
    try { return(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_stmts)).join("; ")) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Lit$Block.f_emit_javascript;  // v8 bug workaround
})();
;// class Lit::Array
if (typeof Lit$Array != 'object') {
  Lit$Array = function() {};
  Lit$Array = new Lit$Array;
  Lit$Array.f_isa = function (s) { return s == 'Lit::Array' };
  Lit$Array.f_perl = function () { return 'Lit::Array.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Lit$Array;
  // accessor array1
  Lit$Array.v_array1 = null;
  Lit$Array.f_array1 = function () { return this.v_array1 }
  // method emit_javascript
  Lit$Array.f_emit_javascript = function () {
    var v_self = this;
    try { var v_needs_interpolation = null;
var List_items = [];
(v_needs_interpolation = 0);(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { (function (v_item) { if ( f_bool(f_and(f_isa(v_item, "Apply"), function () { return (f_or((v_item.f_code() == "circumfix:<( )>"), function () { return (v_item.f_code() == "list:<,>")}))})) ) { (function () { (function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { (function (v_arg) { List_items.push(v_arg); })(a_[i_]) } })((function () { var a = []; (function(a_) { for (var i_ = 0; i_ < a_.length ; i_++) { a.push(a_[i_]) }})(((v_item.f_arguments())));  return a })()); })() } else { (function () { List_items.push(v_item); })() }; })(a_[i_]) } })(v_self.v_array1);(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { (function (v_item) { if ( f_bool(f_or(f_and(f_isa(v_item, "Var"), function () { return (v_item.f_sigil() == "@")}), function () { return f_and(f_isa(v_item, "Apply"), function () { return (f_or((v_item.f_code() == "prefix:<@>"), function () { return (v_item.f_code() == "infix:<..>")}))})})) ) { (function () { (v_needs_interpolation = 1); })() }; })(a_[i_]) } })(List_items);if ( f_bool(v_needs_interpolation) ) { return (function () { var v_s = null;
(v_s = "");(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { (function (v_item) { if ( f_bool(f_or(f_and(f_isa(v_item, "Var"), function () { return (v_item.f_sigil() == "@")}), function () { return f_and(f_isa(v_item, "Apply"), function () { return (f_or((v_item.f_code() == "prefix:<@>"), function () { return (v_item.f_code() == "infix:<..>")}))})})) ) { (function () { (v_s = (f_string(v_s) + f_string("(function(a_) { ") + f_string("for (var i_ = 0; i_ < a_.length ; i_++) { a.push(a_[i_]) }") + f_string("})(") + f_string(v_item.f_emit_javascript()) + f_string("); "))); })() } else { (function () { (v_s = (f_string(v_s) + f_string("a.push(") + f_string(v_item.f_emit_javascript()) + f_string("); "))); })() }; })(a_[i_]) } })(List_items);return((f_string("(function () { var a = []; ") + f_string(v_s) + f_string(" return a })()"))) })() } else { return (function () { return((f_string("[") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(List_items)).join(", ")) + f_string("]"))) })() } } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Lit$Array.f_emit_javascript;  // v8 bug workaround
})();
;// class Lit::Hash
if (typeof Lit$Hash != 'object') {
  Lit$Hash = function() {};
  Lit$Hash = new Lit$Hash;
  Lit$Hash.f_isa = function (s) { return s == 'Lit::Hash' };
  Lit$Hash.f_perl = function () { return 'Lit::Hash.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Lit$Hash;
  // accessor hash1
  Lit$Hash.v_hash1 = null;
  Lit$Hash.f_hash1 = function () { return this.v_hash1 }
  // method emit_javascript
  Lit$Hash.f_emit_javascript = function () {
    var v_self = this;
    try { var List_s = [];
var List_items = [];
(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { (function (v_item) { if ( f_bool(f_and(f_isa(v_item, "Apply"), function () { return (f_or((v_item.f_code() == "circumfix:<( )>"), function () { return (v_item.f_code() == "list:<,>")}))})) ) { (function () { (function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { (function (v_arg) { List_items.push(v_arg); })(a_[i_]) } })((function () { var a = []; (function(a_) { for (var i_ = 0; i_ < a_.length ; i_++) { a.push(a_[i_]) }})(((v_item.f_arguments())));  return a })()); })() } else { (function () { List_items.push(v_item); })() }; })(a_[i_]) } })(v_self.v_hash1);(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { (function (v_item) { if ( f_bool(f_and(f_isa(v_item, "Apply"), function () { return (v_item.f_code() == "infix:<=>>")})) ) { (function () { f_push(List_s, (f_string("a[") + f_string(v_item.f_arguments()[0].f_emit_javascript()) + f_string("] = ") + f_string(v_item.f_arguments()[1].f_emit_javascript()))); })() } else { (function () { if ( f_bool(f_or(f_and(f_isa(v_item, "Var"), function () { return (v_item.f_sigil() == "%")}), function () { return f_and(f_isa(v_item, "Apply"), function () { return (v_item.f_code() == "prefix:<%>")})})) ) { (function () { f_push(List_s, (f_string("(function (o) { ") + f_string("for(var i in o) { ") + f_string("a[i] = o[i] ") + f_string("} ") + f_string("})(") + f_string(v_item.f_emit_javascript()) + f_string(");"))); })() } else { (function () { f_die("Error in hash composer: ", f_perl(v_item)); })() }; })() }; })(a_[i_]) } })(List_items);throw((f_string("(function () { var a = {}; ") + f_string(List_s.join("; ")) + f_string("; ") + f_string("return a ") + f_string("})()"))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Lit$Hash.f_emit_javascript;  // v8 bug workaround
})();
;// class Index
if (typeof Index != 'object') {
  Index = function() {};
  Index = new Index;
  Index.f_isa = function (s) { return s == 'Index' };
  Index.f_perl = function () { return 'Index.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Index;
  // accessor obj
  Index.v_obj = null;
  Index.f_obj = function () { return this.v_obj }
  // accessor index_exp
  Index.v_index_exp = null;
  Index.f_index_exp = function () { return this.v_index_exp }
  // method emit_javascript
  Index.f_emit_javascript = function () {
    var v_self = this;
    try { return((f_string(v_self.v_obj.f_emit_javascript()) + f_string("[") + f_string(v_self.v_index_exp.f_emit_javascript()) + f_string("]"))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Index.f_emit_javascript;  // v8 bug workaround
})();
;// class Lookup
if (typeof Lookup != 'object') {
  Lookup = function() {};
  Lookup = new Lookup;
  Lookup.f_isa = function (s) { return s == 'Lookup' };
  Lookup.f_perl = function () { return 'Lookup.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Lookup;
  // accessor obj
  Lookup.v_obj = null;
  Lookup.f_obj = function () { return this.v_obj }
  // accessor index_exp
  Lookup.v_index_exp = null;
  Lookup.f_index_exp = function () { return this.v_index_exp }
  // method emit_javascript
  Lookup.f_emit_javascript = function () {
    var v_self = this;
    try { var v_str = null;
var v_var = null;
var v_var_js = null;
var v_index_js = null;
(v_str = "");(v_var = v_self.v_obj);if ( f_bool(f_isa(v_var, "Lookup")) ) { (function () { var v_var1 = null;
var v_var1_js = null;
(v_var1 = v_var.f_obj());(v_var1_js = v_var1.f_emit_javascript());(v_str = (f_string(v_str) + f_string("if (") + f_string(v_var1_js) + f_string(" == null) { ") + f_string(v_var1_js) + f_string(" = {} }; ")));(v_var_js = (f_string(v_var1_js) + f_string("[") + f_string(v_var.f_index_exp().f_emit_javascript()) + f_string("]"))); })() } else { (function () { (v_var_js = v_var.f_emit_javascript()); })() };(v_str = (f_string(v_str) + f_string("if (") + f_string(v_var_js) + f_string(" == null) { ") + f_string(v_var_js) + f_string(" = {} }; ")));(v_index_js = v_self.v_index_exp.f_emit_javascript());(v_str = (f_string(v_str) + f_string("return (") + f_string(v_var_js) + f_string("[") + f_string(v_index_js) + f_string("] ") + f_string("); ")));throw((f_string("(function () { ") + f_string(v_str) + f_string("})()"))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Lookup.f_emit_javascript;  // v8 bug workaround
})();
;// class Var
if (typeof Var != 'object') {
  Var = function() {};
  Var = new Var;
  Var.f_isa = function (s) { return s == 'Var' };
  Var.f_perl = function () { return 'Var.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Var;
  // accessor sigil
  Var.v_sigil = null;
  Var.f_sigil = function () { return this.v_sigil }
  // accessor twigil
  Var.v_twigil = null;
  Var.f_twigil = function () { return this.v_twigil }
  // accessor namespace
  Var.v_namespace = null;
  Var.f_namespace = function () { return this.v_namespace }
  // accessor name
  Var.v_name = null;
  Var.f_name = function () { return this.v_name }
  // method emit_javascript
  Var.f_emit_javascript = function () {
    var v_self = this;
    try { var v_table = null;
var v_ns = null;
(v_table = (function () { var a = {}; a["$"] = "v_"; a["@"] = "List_"; a["%"] = "Hash_"; a["&"] = "Code_"; return a })());(v_ns = "");if ( f_bool(v_self.v_namespace) ) { (function () { (v_ns = (f_string(Main.f_to_javascript_namespace(v_self.v_namespace)) + f_string("."))); })() };return(( f_bool(((v_self.v_twigil == "."))) ? ((f_string("v_self.v_") + f_string(v_self.v_name) + f_string(""))) : (( f_bool(((v_self.v_name == "/"))) ? ((f_string((function () { if (v_table == null) { v_table = {} }; return (v_table[v_self.v_sigil] ); })()) + f_string("MATCH"))) : ((f_string((function () { if (v_table == null) { v_table = {} }; return (v_table[v_self.v_sigil] ); })()) + f_string(v_ns) + f_string(v_self.v_name))))))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Var.f_emit_javascript;  // v8 bug workaround
  // method plain_name
  Var.f_plain_name = function () {
    var v_self = this;
    try { if ( f_bool(v_self.v_namespace) ) { (function () { throw((f_string(v_self.v_namespace) + f_string(".") + f_string(v_self.v_name))); })() };throw(v_self.v_name) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Var.f_plain_name;  // v8 bug workaround
})();
;// class Proto
if (typeof Proto != 'object') {
  Proto = function() {};
  Proto = new Proto;
  Proto.f_isa = function (s) { return s == 'Proto' };
  Proto.f_perl = function () { return 'Proto.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Proto;
  // accessor name
  Proto.v_name = null;
  Proto.f_name = function () { return this.v_name }
  // method emit_javascript
  Proto.f_emit_javascript = function () {
    var v_self = this;
    try { return(Main.f_to_javascript_namespace(v_self.v_name)) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Proto.f_emit_javascript;  // v8 bug workaround
})();
;// class Call
if (typeof Call != 'object') {
  Call = function() {};
  Call = new Call;
  Call.f_isa = function (s) { return s == 'Call' };
  Call.f_perl = function () { return 'Call.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Call;
  // accessor invocant
  Call.v_invocant = null;
  Call.f_invocant = function () { return this.v_invocant }
  // accessor hyper
  Call.v_hyper = null;
  Call.f_hyper = function () { return this.v_hyper }
  // accessor method
  Call.v_method = null;
  Call.f_method = function () { return this.v_method }
  // accessor arguments
  Call.v_arguments = null;
  Call.f_arguments = function () { return this.v_arguments }
  // method emit_javascript
  Call.f_emit_javascript = function () {
    var v_self = this;
    try { var v_invocant = null;
var v_meth = null;
(v_invocant = v_self.v_invocant.f_emit_javascript());if ( f_bool((v_invocant == "self")) ) { (function () { (v_invocant = "v_self"); })() };if ( f_bool((v_self.v_method == "new")) ) { (function () { var v_str = null;
(v_str = []);(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { (function (v_field) { if ( f_bool(f_and(f_isa(v_field, "Apply"), function () { return (v_field.f_code() == "infix:<=>>")})) ) { (function () { v_str.push((f_string("v_") + f_string(v_field.f_arguments()[0].f_buf()) + f_string(": ") + f_string(v_field.f_arguments()[1].f_emit_javascript()))); })() } else { (function () { f_die("Error in constructor, field: ", f_perl(v_field)); })() }; })(a_[i_]) } })(v_self.v_arguments);throw((f_string("(function () { ") + f_string("var tmp = {") + f_string(v_str.join(",")) + f_string("}; ") + f_string("tmp.__proto__ = ") + f_string(Main.f_to_javascript_namespace(v_invocant)) + f_string("; ") + f_string("return tmp ") + f_string("})()"))); })() };if ( f_bool(f_or(f_or(f_or(f_or(f_or(f_or(f_or(((v_self.v_method == "perl")), function () { return ((v_self.v_method == "isa"))}), function () { return ((v_self.v_method == "scalar"))}), function () { return ((v_self.v_method == "keys"))}), function () { return ((v_self.v_method == "values"))}), function () { return ((v_self.v_method == "elems"))}), function () { return ((v_self.v_method == "say"))}), function () { return ((v_self.v_method == "chars"))})) ) { (function () { if ( f_bool((v_self.v_hyper)) ) { (function () { throw((f_string("(function (a_) { ") + f_string("var out = []; ") + f_string("if ( a_ == null ) { return out }; ") + f_string("for(var i = 0; i < a_.length; i++) { ") + f_string("out.push( f_") + f_string(v_self.v_method) + f_string("(a_[i]) ) } return out;") + f_string(" })(") + f_string(v_invocant) + f_string(")"))); })() };throw((f_string("f_") + f_string(v_self.v_method) + f_string("(") + f_string(v_invocant) + f_string((( f_bool(v_self.v_arguments) ? (f_string(", ") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(", "))) : ""))) + f_string(")"))); })() };if ( f_bool(f_or(f_or(f_or(f_or(((v_self.v_method == "join")), function () { return ((v_self.v_method == "shift"))}), function () { return ((v_self.v_method == "unshift"))}), function () { return ((v_self.v_method == "push"))}), function () { return ((v_self.v_method == "pop"))})) ) { (function () { throw((f_string(v_invocant) + f_string(".") + f_string(v_self.v_method) + f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(", ")) + f_string(")"))); })() };(v_meth = v_self.v_method);if ( f_bool((v_self.v_hyper)) ) { (function () { throw((f_string("(function (a_) { ") + f_string("var out = []; ") + f_string("if ( a_ == null ) { return out }; ") + f_string("for(var i = 0; i < a_.length; i++) { ") + f_string("out.push( a_[i].f_") + f_string(v_meth) + f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(", ")) + f_string(") ) ") + f_string("}; ") + f_string("return out;") + f_string(" })(") + f_string(v_invocant) + f_string(")"))); })() };if ( f_bool((v_meth == "postcircumfix:<( )>")) ) { (function () { throw((f_string("(") + f_string(v_invocant) + f_string(")(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(", ")) + f_string(")"))); })() };throw((f_string(v_invocant) + f_string(".f_") + f_string(v_meth) + f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(", ")) + f_string(")"))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Call.f_emit_javascript;  // v8 bug workaround
})();
;// class Apply
if (typeof Apply != 'object') {
  Apply = function() {};
  Apply = new Apply;
  Apply.f_isa = function (s) { return s == 'Apply' };
  Apply.f_perl = function () { return 'Apply.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Apply;
  // accessor code
  Apply.v_code = null;
  Apply.f_code = function () { return this.v_code }
  // accessor arguments
  Apply.v_arguments = null;
  Apply.f_arguments = function () { return this.v_arguments }
  // accessor namespace
  Apply.v_namespace = null;
  Apply.f_namespace = function () { return this.v_namespace }
  // method emit_javascript
  Apply.f_emit_javascript = function () {
    var v_self = this;
    try { var v_code = null;
(v_code = v_self.v_code);if ( f_bool(f_isa(v_code, "Str")) ) { (function () {  })() } else { (function () { throw((f_string("(") + f_string(v_self.v_code.f_emit_javascript()) + f_string(")->(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit() ) }; return out; })(v_self.v_arguments)).join(", ")) + f_string(")"))); })() };if ( f_bool((v_code == "self")) ) { (function () { throw("v_self"); })() };if ( f_bool((v_code == "Mu")) ) { (function () { throw("null"); })() };if ( f_bool((v_code == "make")) ) { (function () { throw((f_string("(v_MATCH.v_capture = ") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(", ")) + f_string(")"))); })() };if ( f_bool((v_code == "defined")) ) { (function () { throw((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" ")) + f_string(" != null)"))); })() };if ( f_bool((v_code == "substr")) ) { (function () { throw((f_string("(") + f_string((v_self.v_arguments[0]).f_emit_javascript()) + f_string(" || \"\").substr(") + f_string((v_self.v_arguments[1]).f_emit_javascript()) + f_string(", ") + f_string((v_self.v_arguments[2]).f_emit_javascript()) + f_string(")"))); })() };if ( f_bool((v_code == "Int")) ) { (function () { throw((f_string("parseInt(") + f_string((v_self.v_arguments[0]).f_emit_javascript()) + f_string(")"))); })() };if ( f_bool((v_code == "Num")) ) { (function () { throw((f_string("parseFloat(") + f_string((v_self.v_arguments[0]).f_emit_javascript()) + f_string(")"))); })() };if ( f_bool((v_code == "prefix:<~>")) ) { (function () { throw((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" ")) + f_string(").f_string()"))); })() };if ( f_bool((v_code == "prefix:<!>")) ) { (function () { throw((f_string("( f_bool(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" ")) + f_string(") ? false : true)"))); })() };if ( f_bool((v_code == "prefix:<?>")) ) { (function () { throw((f_string("( f_bool(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" ")) + f_string(") ? true : false)"))); })() };if ( f_bool((v_code == "prefix:<$>")) ) { (function () { throw((f_string("f_scalar(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" ")) + f_string(")"))); })() };if ( f_bool((v_code == "prefix:<@>")) ) { (function () { throw((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" ")) + f_string(")"))); })() };if ( f_bool((v_code == "prefix:<%>")) ) { (function () { throw((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" ")) + f_string(").f_hash()"))); })() };if ( f_bool((v_code == "postfix:<++>")) ) { (function () { throw((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" ")) + f_string(")++"))); })() };if ( f_bool((v_code == "postfix:<-->")) ) { (function () { throw((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" ")) + f_string(")--"))); })() };if ( f_bool((v_code == "prefix:<++>")) ) { (function () { throw((f_string("++(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" ")) + f_string(")"))); })() };if ( f_bool((v_code == "prefix:<-->")) ) { (function () { throw((f_string("--(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" ")) + f_string(")"))); })() };if ( f_bool((v_code == "list:<~>")) ) { (function () { throw((f_string("(f_string(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(") + f_string(")) + f_string("))"))); })() };if ( f_bool((v_code == "infix:<+>")) ) { (function () { throw((f_string("f_add(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(", ")) + f_string(")"))); })() };if ( f_bool((v_code == "infix:<->")) ) { (function () { throw((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" - ")) + f_string(")"))); })() };if ( f_bool((v_code == "infix:<*>")) ) { (function () { throw((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" * ")) + f_string(")"))); })() };if ( f_bool((v_code == "infix:</>")) ) { (function () { throw((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" / ")) + f_string(")"))); })() };if ( f_bool((v_code == "infix:<>>")) ) { (function () { throw((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" > ")) + f_string(")"))); })() };if ( f_bool((v_code == "infix:<<>")) ) { (function () { throw((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" < ")) + f_string(")"))); })() };if ( f_bool((v_code == "infix:<>=>")) ) { (function () { throw((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" >= ")) + f_string(")"))); })() };if ( f_bool((v_code == "infix:<<=>")) ) { (function () { throw((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" <= ")) + f_string(")"))); })() };if ( f_bool((v_code == "infix:<=>>")) ) { (function () { throw((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(", ")) + f_string(")"))); })() };if ( f_bool((v_code == "infix:<..>")) ) { (function () { throw((f_string("(function (a) { ") + f_string("for (var i=") + f_string(v_self.v_arguments[0].f_emit_javascript()) + f_string(", l=") + f_string(v_self.v_arguments[1].f_emit_javascript()) + f_string("; ") + f_string("i<=l; ++i)") + f_string("{ ") + f_string("a.push(i) ") + f_string("}; ") + f_string("return a ") + f_string("})([])"))); })() };if ( f_bool((v_code == "infix:<&&>")) ) { (function () { throw((f_string("f_and(") + f_string(v_self.v_arguments[0].f_emit_javascript()) + f_string(", ") + f_string("function () { return ") + f_string(v_self.v_arguments[1].f_emit_javascript()) + f_string("})"))); })() };if ( f_bool((v_code == "infix:<||>")) ) { (function () { throw((f_string("f_or(") + f_string(v_self.v_arguments[0].f_emit_javascript()) + f_string(", ") + f_string("function () { return ") + f_string(v_self.v_arguments[1].f_emit_javascript()) + f_string("})"))); })() };if ( f_bool((v_code == "infix:<//>")) ) { (function () { throw((f_string("f_defined_or(") + f_string(v_self.v_arguments[0].f_emit_javascript()) + f_string(", ") + f_string("function () { return ") + f_string(v_self.v_arguments[1].f_emit_javascript()) + f_string("})"))); })() };if ( f_bool((v_code == "infix:<eq>")) ) { (function () { throw((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" == ")) + f_string(")"))); })() };if ( f_bool((v_code == "infix:<ne>")) ) { (function () { throw((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" != ")) + f_string(")"))); })() };if ( f_bool((v_code == "infix:<ge>")) ) { (function () { throw((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" >= ")) + f_string(")"))); })() };if ( f_bool((v_code == "infix:<le>")) ) { (function () { throw((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" <= ")) + f_string(")"))); })() };if ( f_bool((v_code == "infix:<==>")) ) { (function () { throw((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" == ")) + f_string(")"))); })() };if ( f_bool((v_code == "infix:<!=>")) ) { (function () { throw((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(" != ")) + f_string(")"))); })() };if ( f_bool((v_code == "exists")) ) { (function () { var v_arg = null;
(v_arg = v_self.v_arguments[0]);if ( f_bool(f_isa(v_arg, "Lookup")) ) { (function () { throw((f_string("(") + f_string((v_arg.f_obj()).f_emit_javascript()) + f_string(").hasOwnProperty(") + f_string((v_arg.f_index_exp()).f_emit_javascript()) + f_string(")"))); })() }; })() };if ( f_bool((v_code == "ternary:<?? !!>")) ) { (function () { throw((f_string("( f_bool(") + f_string((v_self.v_arguments[0]).f_emit_javascript()) + f_string(")") + f_string(" ? ") + f_string((v_self.v_arguments[1]).f_emit_javascript()) + f_string(" : ") + f_string((v_self.v_arguments[2]).f_emit_javascript()) + f_string(")"))); })() };if ( f_bool((v_code == "circumfix:<( )>")) ) { (function () { throw((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(", ")) + f_string(")"))); })() };if ( f_bool((v_code == "infix:<=>")) ) { (function () { throw(v__NAMESPACE.f_emit_bind(v_self.v_arguments[0], v_self.v_arguments[1])); })() };if ( f_bool((v_code == "return")) ) { (function () { throw((f_string("throw(") + f_string((( f_bool(v_self.v_arguments) ? v_self.v_arguments[0].f_emit_javascript() : "null"))) + f_string(")"))); })() };(v_code = (f_string("f_") + f_string(v_self.v_code)));if ( f_bool(v_self.v_namespace) ) { (function () { (v_code = (f_string(Main.f_to_javascript_namespace(v_self.v_namespace)) + f_string(".") + f_string(v_code))); })() } else { (function () { if ( f_bool(f_and(f_and(f_and(f_and(f_and(f_and(f_and(f_and(f_and(((v_code != "f_index")), function () { return ((v_code != "f_die"))}), function () { return ((v_code != "f_shift"))}), function () { return ((v_code != "f_unshift"))}), function () { return ((v_code != "f_push"))}), function () { return ((v_code != "f_pop"))}), function () { return ((v_code != "f_chr"))}), function () { return ((v_code != "f_say"))}), function () { return ((v_code != "f_print"))}), function () { return ((v_code != "f_warn"))})) ) { (function () { (v_code = (f_string("v__NAMESPACE.") + f_string(v_code))); })() }; })() };return((f_string(v_code) + f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_self.v_arguments)).join(", ")) + f_string(")"))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Apply.f_emit_javascript;  // v8 bug workaround
  // sub emit_bind
  Apply.f_emit_bind = function (v_parameters, v_arguments) {
    try { if ( f_bool(f_isa(v_parameters, "Lit::Array")) ) { (function () { var v_a = null;
var v_str = null;
var v_i = null;
(v_a = v_parameters.f_array1());(v_str = "do { ");(v_i = 0);(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { (function (v_var) { (v_str = (f_string(v_str) + f_string(" ") + f_string(v__NAMESPACE.f_emit_bind(v_var, (function () { var tmp = {v_obj: v_arguments,v_index_exp: (function () { var tmp = {v_int: v_i}; tmp.__proto__ = Val$Int; return tmp })()}; tmp.__proto__ = Index; return tmp })())) + f_string("; ")));(v_i = f_add(v_i, 1)); })(a_[i_]) } })((function () { var a = []; (function(a_) { for (var i_ = 0; i_ < a_.length ; i_++) { a.push(a_[i_]) }})((v_a));  return a })());throw((f_string(v_str) + f_string(v_parameters.f_emit_javascript()) + f_string(" }"))); })() };if ( f_bool(f_isa(v_parameters, "Lit::Hash")) ) { (function () { var v_a = null;
var v_b = null;
var v_str = null;
var v_i = null;
var v_arg = null;
(v_a = v_parameters.f_hash1());(v_b = v_arguments.f_hash1());(v_str = "do { ");(v_i = 0);(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { (function (v_var) { (v_arg = (function () { var tmp = {v_code: "Mu"}; tmp.__proto__ = Apply; return tmp })());(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { (function (v_var2) { if ( f_bool(((v_var2[0]).f_buf() == (v_var[0]).f_buf())) ) { (function () { (v_arg = v_var2[1]); })() }; })(a_[i_]) } })((function () { var a = []; (function(a_) { for (var i_ = 0; i_ < a_.length ; i_++) { a.push(a_[i_]) }})((v_b));  return a })());(v_str = (f_string(v_str) + f_string(" ") + f_string(v__NAMESPACE.f_emit_bind(v_var[1], v_arg)) + f_string("; ")));(v_i = f_add(v_i, 1)); })(a_[i_]) } })((function () { var a = []; (function(a_) { for (var i_ = 0; i_ < a_.length ; i_++) { a.push(a_[i_]) }})((v_a));  return a })());throw((f_string(v_str) + f_string(v_parameters.f_emit_javascript()) + f_string(" }"))); })() };if ( f_bool(f_isa(v_parameters, "Call")) ) { (function () { throw((f_string("(") + f_string((v_parameters.f_invocant()).f_emit_javascript()) + f_string(".v_") + f_string(v_parameters.f_method()) + f_string(" = ") + f_string(v_arguments.f_emit_javascript()) + f_string(")"))); })() };if ( f_bool(f_isa(v_parameters, "Lookup")) ) { (function () { var v_str = null;
var v_var = null;
var v_var_js = null;
var v_index_js = null;
(v_str = "");(v_var = v_parameters.f_obj());if ( f_bool(f_isa(v_var, "Lookup")) ) { (function () { var v_var1 = null;
var v_var1_js = null;
(v_var1 = v_var.f_obj());(v_var1_js = v_var1.f_emit_javascript());(v_str = (f_string(v_str) + f_string("if (") + f_string(v_var1_js) + f_string(" == null) { ") + f_string(v_var1_js) + f_string(" = {} }; ")));(v_var_js = (f_string(v_var1_js) + f_string("[") + f_string(v_var.f_index_exp().f_emit_javascript()) + f_string("]"))); })() } else { (function () { (v_var_js = v_var.f_emit_javascript()); })() };(v_str = (f_string(v_str) + f_string("if (") + f_string(v_var_js) + f_string(" == null) { ") + f_string(v_var_js) + f_string(" = {} }; ")));(v_index_js = v_parameters.f_index_exp().f_emit_javascript());(v_str = (f_string(v_str) + f_string("return (") + f_string(v_var_js) + f_string("[") + f_string(v_index_js) + f_string("] ") + f_string(" = ") + f_string(v_arguments.f_emit_javascript()) + f_string("); ")));throw((f_string("(function () { ") + f_string(v_str) + f_string("})()"))); })() };if ( f_bool(f_or(f_and(f_isa(v_parameters, "Var"), function () { return (v_parameters.f_sigil() == "@")}), function () { return f_and(f_isa(v_parameters, "Decl"), function () { return (v_parameters.f_var().f_sigil() == "@")})})) ) { (function () { (v_arguments = (function () { var tmp = {v_array1: [v_arguments]}; tmp.__proto__ = Lit$Array; return tmp })()); })() } else { (function () { if ( f_bool(f_or(f_and(f_isa(v_parameters, "Var"), function () { return (v_parameters.f_sigil() == "%")}), function () { return f_and(f_isa(v_parameters, "Decl"), function () { return (v_parameters.f_var().f_sigil() == "%")})})) ) { (function () { (v_arguments = (function () { var tmp = {v_hash1: [v_arguments]}; tmp.__proto__ = Lit$Hash; return tmp })()); })() }; })() };return((f_string("(") + f_string(v_parameters.f_emit_javascript()) + f_string(" = ") + f_string(v_arguments.f_emit_javascript()) + f_string(")"))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
})();
;// class If
if (typeof If != 'object') {
  If = function() {};
  If = new If;
  If.f_isa = function (s) { return s == 'If' };
  If.f_perl = function () { return 'If.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = If;
  // accessor cond
  If.v_cond = null;
  If.f_cond = function () { return this.v_cond }
  // accessor body
  If.v_body = null;
  If.f_body = function () { return this.v_body }
  // accessor otherwise
  If.v_otherwise = null;
  If.f_otherwise = function () { return this.v_otherwise }
  // method emit_javascript
  If.f_emit_javascript = function () {
    var v_self = this;
    try { var v_cond = null;
var v_body = null;
var v_s = null;
(v_cond = v_self.v_cond);if ( f_bool(f_and(f_isa(v_cond, "Var"), function () { return (v_cond.f_sigil() == "@")})) ) { (function () { (v_cond = (function () { var tmp = {v_code: "prefix:<@>",v_arguments: [v_cond]}; tmp.__proto__ = Apply; return tmp })()); })() };(v_body = (function () { var tmp = {v_block: v_self.v_body.f_stmts(),v_needs_return: 0}; tmp.__proto__ = Perlito$Javascript$LexicalBlock; return tmp })());(v_s = (f_string("if ( f_bool(") + f_string(v_cond.f_emit_javascript()) + f_string(") ) { ") + f_string("(function () { ") + f_string(v_body.f_emit_javascript()) + f_string(" })() }")));if ( f_bool(v_self.v_otherwise) ) { (function () { var v_otherwise = null;
(v_otherwise = (function () { var tmp = {v_block: v_self.v_otherwise.f_stmts(),v_needs_return: 0}; tmp.__proto__ = Perlito$Javascript$LexicalBlock; return tmp })());(v_s = (f_string(v_s) + f_string(" else { ") + f_string("(function () { ") + f_string(v_otherwise.f_emit_javascript()) + f_string(" })() }"))); })() };throw(v_s) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  If.f_emit_javascript;  // v8 bug workaround
})();
;// class While
if (typeof While != 'object') {
  While = function() {};
  While = new While;
  While.f_isa = function (s) { return s == 'While' };
  While.f_perl = function () { return 'While.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = While;
  // accessor init
  While.v_init = null;
  While.f_init = function () { return this.v_init }
  // accessor cond
  While.v_cond = null;
  While.f_cond = function () { return this.v_cond }
  // accessor continue
  While.v_continue = null;
  While.f_continue = function () { return this.v_continue }
  // accessor body
  While.v_body = null;
  While.f_body = function () { return this.v_body }
  // method emit_javascript
  While.f_emit_javascript = function () {
    var v_self = this;
    try { var v_body = null;
(v_body = (function () { var tmp = {v_block: v_self.v_body.f_stmts(),v_needs_return: 0}; tmp.__proto__ = Perlito$Javascript$LexicalBlock; return tmp })());throw((f_string("for ( ") + f_string((( f_bool(v_self.v_init) ? (f_string(v_self.v_init.f_emit_javascript()) + f_string("; ")) : "; "))) + f_string((( f_bool(v_self.v_cond) ? (f_string("f_bool(") + f_string(v_self.v_cond.f_emit_javascript()) + f_string("); ")) : "; "))) + f_string((( f_bool(v_self.v_continue) ? (f_string(v_self.v_continue.f_emit_javascript()) + f_string(" ")) : " "))) + f_string(") { ") + f_string("(function () { ") + f_string(v_body.f_emit_javascript()) + f_string(" })()") + f_string(" }"))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  While.f_emit_javascript;  // v8 bug workaround
})();
;// class For
if (typeof For != 'object') {
  For = function() {};
  For = new For;
  For.f_isa = function (s) { return s == 'For' };
  For.f_perl = function () { return 'For.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = For;
  // accessor cond
  For.v_cond = null;
  For.f_cond = function () { return this.v_cond }
  // accessor body
  For.v_body = null;
  For.f_body = function () { return this.v_body }
  // method emit_javascript
  For.f_emit_javascript = function () {
    var v_self = this;
    try { var v_cond = null;
var v_body = null;
var v_sig = null;
(v_cond = v_self.v_cond);if ( f_bool(( f_bool((f_and(f_isa(v_cond, "Var"), function () { return (v_cond.f_sigil() == "@")}))) ? false : true)) ) { (function () { (v_cond = (function () { var tmp = {v_array1: [v_cond]}; tmp.__proto__ = Lit$Array; return tmp })()); })() };(v_body = (function () { var tmp = {v_block: v_self.v_body.f_stmts(),v_needs_return: 0}; tmp.__proto__ = Perlito$Javascript$LexicalBlock; return tmp })());(v_sig = "v__");if ( f_bool(v_self.v_body.f_sig()) ) { (function () { (v_sig = v_self.v_body.f_sig().f_emit_javascript()); })() };return((f_string("(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { ") + f_string((f_string("(function (") + f_string(v_sig) + f_string(") ") + f_string("{") + f_string(" "))) + f_string(v_body.f_emit_javascript()) + f_string(" })(a_[i_]) } })") + f_string("(") + f_string(v_cond.f_emit_javascript()) + f_string(")"))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  For.f_emit_javascript;  // v8 bug workaround
})();
;// class Decl
if (typeof Decl != 'object') {
  Decl = function() {};
  Decl = new Decl;
  Decl.f_isa = function (s) { return s == 'Decl' };
  Decl.f_perl = function () { return 'Decl.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Decl;
  // accessor decl
  Decl.v_decl = null;
  Decl.f_decl = function () { return this.v_decl }
  // accessor type
  Decl.v_type = null;
  Decl.f_type = function () { return this.v_type }
  // accessor var
  Decl.v_var = null;
  Decl.f_var = function () { return this.v_var }
  // method emit_javascript
  Decl.f_emit_javascript = function () {
    var v_self = this;
    try { return(v_self.v_var.f_emit_javascript()) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Decl.f_emit_javascript;  // v8 bug workaround
  // method emit_javascript_init
  Decl.f_emit_javascript_init = function () {
    var v_self = this;
    try { if ( f_bool((v_self.v_decl == "my")) ) { return (function () { var v_str = null;
(v_str = "");(v_str = (f_string(v_str) + f_string("var ") + f_string((v_self.v_var).f_emit_javascript()) + f_string(" = ")));if ( f_bool(((v_self.v_var).f_sigil() == "%")) ) { (function () { (v_str = (f_string(v_str) + f_string("{};") + f_string((f_string("\n"))))); })() } else { (function () { if ( f_bool(((v_self.v_var).f_sigil() == "@")) ) { (function () { (v_str = (f_string(v_str) + f_string("[];") + f_string((f_string("\n"))))); })() } else { (function () { (v_str = (f_string(v_str) + f_string("null;") + f_string((f_string("\n"))))); })() }; })() };throw(v_str) })() } else { return (function () { return(f_die((f_string("not implemented: Decl '") + f_string(v_self.v_decl) + f_string((f_string("'")))))) })() } } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Decl.f_emit_javascript_init;  // v8 bug workaround
})();
;// class Sig
if (typeof Sig != 'object') {
  Sig = function() {};
  Sig = new Sig;
  Sig.f_isa = function (s) { return s == 'Sig' };
  Sig.f_perl = function () { return 'Sig.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Sig;
  // accessor invocant
  Sig.v_invocant = null;
  Sig.f_invocant = function () { return this.v_invocant }
  // accessor positional
  Sig.v_positional = null;
  Sig.f_positional = function () { return this.v_positional }
  // accessor named
  Sig.v_named = null;
  Sig.f_named = function () { return this.v_named }
})();
;// class Method
if (typeof Method != 'object') {
  Method = function() {};
  Method = new Method;
  Method.f_isa = function (s) { return s == 'Method' };
  Method.f_perl = function () { return 'Method.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Method;
  // accessor name
  Method.v_name = null;
  Method.f_name = function () { return this.v_name }
  // accessor sig
  Method.v_sig = null;
  Method.f_sig = function () { return this.v_sig }
  // accessor block
  Method.v_block = null;
  Method.f_block = function () { return this.v_block }
  // method emit_javascript
  Method.f_emit_javascript = function () {
    var v_self = this;
    try { var v_sig = null;
var v_invocant = null;
var v_pos = null;
var v_str = null;
(v_sig = v_self.v_sig);(v_invocant = v_sig.f_invocant());(v_pos = v_sig.f_positional());(v_str = (function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_pos).join(", "));return((f_string("function ") + f_string(v_self.v_name) + f_string("(") + f_string(v_str) + f_string(") { ") + f_string(((function () { var tmp = {v_block: v_self.v_block,v_needs_return: 1,v_top_level: 1}; tmp.__proto__ = Perlito$Javascript$LexicalBlock; return tmp })()).f_emit_javascript()) + f_string(" }"))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Method.f_emit_javascript;  // v8 bug workaround
})();
;// class Sub
if (typeof Sub != 'object') {
  Sub = function() {};
  Sub = new Sub;
  Sub.f_isa = function (s) { return s == 'Sub' };
  Sub.f_perl = function () { return 'Sub.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Sub;
  // accessor name
  Sub.v_name = null;
  Sub.f_name = function () { return this.v_name }
  // accessor sig
  Sub.v_sig = null;
  Sub.f_sig = function () { return this.v_sig }
  // accessor block
  Sub.v_block = null;
  Sub.f_block = function () { return this.v_block }
  // method emit_javascript
  Sub.f_emit_javascript = function () {
    var v_self = this;
    try { var v_sig = null;
var v_pos = null;
var v_str = null;
(v_sig = v_self.v_sig);(v_pos = v_sig.f_positional());(v_str = (function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_javascript() ) }; return out; })(v_pos).join(", "));return((f_string("function ") + f_string(v_self.v_name) + f_string("(") + f_string(v_str) + f_string(") { ") + f_string(((function () { var tmp = {v_block: v_self.v_block,v_needs_return: 1,v_top_level: 1}; tmp.__proto__ = Perlito$Javascript$LexicalBlock; return tmp })()).f_emit_javascript()) + f_string(" }"))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Sub.f_emit_javascript;  // v8 bug workaround
})();
;// class Do
if (typeof Do != 'object') {
  Do = function() {};
  Do = new Do;
  Do.f_isa = function (s) { return s == 'Do' };
  Do.f_perl = function () { return 'Do.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Do;
  // accessor block
  Do.v_block = null;
  Do.f_block = function () { return this.v_block }
  // method emit_javascript
  Do.f_emit_javascript = function () {
    var v_self = this;
    try { if ( f_bool(f_isa(v_self.v_block, "Do")) ) { (function () { throw(v_self.v_block.f_emit_javascript()); })() };if ( f_bool(f_isa(v_self.v_block, "Lit::Block")) ) { (function () { throw((f_string("(function () { ") + f_string(((function () { var tmp = {v_block: v_self.v_block.f_stmts(),v_needs_return: 1}; tmp.__proto__ = Perlito$Javascript$LexicalBlock; return tmp })()).f_emit_javascript()) + f_string(" })()"))); })() };throw((f_string("(function () { ") + f_string(((function () { var tmp = {v_block: v_self.v_block,v_needs_return: 1}; tmp.__proto__ = Perlito$Javascript$LexicalBlock; return tmp })()).f_emit_javascript()) + f_string(" })()"))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Do.f_emit_javascript;  // v8 bug workaround
})();
;// class Use
if (typeof Use != 'object') {
  Use = function() {};
  Use = new Use;
  Use.f_isa = function (s) { return s == 'Use' };
  Use.f_perl = function () { return 'Use.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Use;
  // accessor mod
  Use.v_mod = null;
  Use.f_mod = function () { return this.v_mod }
  // method emit_javascript
  Use.f_emit_javascript = function () {
    var v_self = this;
    try { return((f_string("// use ") + f_string(v_self.v_mod) + f_string((f_string("\n"))))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Use.f_emit_javascript;  // v8 bug workaround
})();
;})();

// class GLOBAL
if (typeof GLOBAL != 'object') {
  GLOBAL = function() {};
  GLOBAL = new GLOBAL;
  GLOBAL.f_isa = function (s) { return s == 'GLOBAL' };
  GLOBAL.f_perl = function () { return 'GLOBAL.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = GLOBAL;
// use v6
;// class Perlito::Grammar
if (typeof Perlito$Grammar != 'object') {
  Perlito$Grammar = function() {};
  Perlito$Grammar = new Perlito$Grammar;
  Perlito$Grammar.f_isa = function (s) { return s == 'Perlito::Grammar' };
  Perlito$Grammar.f_perl = function () { return 'Perlito::Grammar.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Perlito$Grammar;
  // method ident
  Perlito$Grammar.f_ident = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((((function () { var v_m2 = null;
(v_m2 = v_grammar.f_digit(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? false : true));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })()), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_word(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("_" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())})) })()), function () { return ((function () { var v_last_match_null = null;
var v_last_pos = null;
(v_last_match_null = 0);(v_last_pos = v_MATCH.f_to());for ( ; f_bool(f_and(((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(((function () { return((((function () { var v_m2 = null;
(v_m2 = v_grammar.f_word(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("_" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((((function () { var v_m2 = null;
(v_m2 = v_grammar.f_digit(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()))) })())})) })()), function () { return ((v_last_match_null < 2))}));  ) { (function () { if ( f_bool((v_last_pos == v_MATCH.f_to())) ) { (function () { (v_last_match_null = f_add(v_last_match_null, 1)); })() } else { (function () { (v_last_match_null = 0); })() };(v_last_pos = v_MATCH.f_to()); })() };(v_MATCH.v_to = v_last_pos);return(1) })())}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_ident;  // v8 bug workaround
  // method full_ident
  Perlito$Grammar.f_full_ident = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_last_match_null = null;
var v_last_pos = null;
(v_last_match_null = 0);(v_last_pos = v_MATCH.f_to());for ( ; f_bool(f_and(((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and((f_and(("::" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}))) })())) })()), function () { return ((v_last_match_null < 2))}));  ) { (function () { if ( f_bool((v_last_pos == v_MATCH.f_to())) ) { (function () { (v_last_match_null = f_add(v_last_match_null, 1)); })() } else { (function () { (v_last_match_null = 0); })() };(v_last_pos = v_MATCH.f_to()); })() };(v_MATCH.v_to = v_last_pos);return(1) })())}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_full_ident;  // v8 bug workaround
  // method namespace_before_ident
  Perlito$Grammar.f_namespace_before_ident = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return(((f_and(("::" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))})))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? true : false));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })())}), function () { return ((function () { var v_last_match_null = null;
var v_last_pos = null;
(v_last_match_null = 0);(v_last_pos = v_MATCH.f_to());for ( ; f_bool(f_and(((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and((f_and(("::" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return(((f_and(("::" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))})))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? true : false));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })())}))) })())) })()), function () { return ((v_last_match_null < 2))}));  ) { (function () { if ( f_bool((v_last_pos == v_MATCH.f_to())) ) { (function () { (v_last_match_null = f_add(v_last_match_null, 1)); })() } else { (function () { (v_last_match_null = 0); })() };(v_last_pos = v_MATCH.f_to()); })() };(v_MATCH.v_to = v_last_pos);return(1) })())}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_namespace_before_ident;  // v8 bug workaround
  // method optional_namespace_before_ident
  Perlito$Grammar.f_optional_namespace_before_ident = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_namespace_before_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["namespace_before_ident"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_and(("::" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["namespace_before_ident"] ); })()).f_string())) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(1, function () { return (f_or(((function () { return((v_MATCH.v_capture = "")) })()), function () { return 1}))}))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_optional_namespace_before_ident;  // v8 bug workaround
  // method pod_begin
  Perlito$Grammar.f_pod_begin = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_is_newline(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_and(("=end" == (v_str || "").substr(v_MATCH.f_to(), 4)), function () { return ((v_MATCH.v_to = f_add(4, v_MATCH.f_to())))}))}), function () { return ((function () { var v_last_match_null = null;
var v_last_pos = null;
(v_last_match_null = 0);(v_last_pos = v_MATCH.f_to());for ( ; f_bool(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_not_newline(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((v_last_match_null < 2))}));  ) { (function () { if ( f_bool((v_last_pos == v_MATCH.f_to())) ) { (function () { (v_last_match_null = f_add(v_last_match_null, 1)); })() } else { (function () { (v_last_match_null = 0); })() };(v_last_pos = v_MATCH.f_to()); })() };(v_MATCH.v_to = v_last_pos);return(1) })())}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and((f_and(("" != (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_last_match_null = null;
var v_last_pos = null;
(v_last_match_null = 0);(v_last_pos = v_MATCH.f_to());for ( ; f_bool(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_not_newline(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((v_last_match_null < 2))}));  ) { (function () { if ( f_bool((v_last_pos == v_MATCH.f_to())) ) { (function () { (v_last_match_null = f_add(v_last_match_null, 1)); })() } else { (function () { (v_last_match_null = 0); })() };(v_last_pos = v_MATCH.f_to()); })() };(v_MATCH.v_to = v_last_pos);return(1) })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_pod_begin(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_pod_begin;  // v8 bug workaround
  // method ws
  Perlito$Grammar.f_ws = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((((function () { var v_last_match_null = null;
var v_last_pos = null;
var v_count = null;
(v_last_match_null = 0);(v_last_pos = v_MATCH.f_to());(v_count = 0);for ( ; f_bool(f_and(((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(((function () { return((f_and((f_and(("#" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_last_match_null = null;
var v_last_pos = null;
(v_last_match_null = 0);(v_last_pos = v_MATCH.f_to());for ( ; f_bool(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_not_newline(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((v_last_match_null < 2))}));  ) { (function () { if ( f_bool((v_last_pos == v_MATCH.f_to())) ) { (function () { (v_last_match_null = f_add(v_last_match_null, 1)); })() } else { (function () { (v_last_match_null = 0); })() };(v_last_pos = v_MATCH.f_to()); })() };(v_MATCH.v_to = v_last_pos);return(1) })())}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_is_newline(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(((function () { return((f_and((f_and(("=begin" == (v_str || "").substr(v_MATCH.f_to(), 6)), function () { return ((v_MATCH.v_to = f_add(6, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_pod_begin(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and((f_and(("=for" == (v_str || "").substr(v_MATCH.f_to(), 4)), function () { return ((v_MATCH.v_to = f_add(4, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_pod_begin(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((1)) })())})) })())}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((((function () { var v_m2 = null;
(v_m2 = v_grammar.f_space(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()))) })())})) })()), function () { return ((v_last_match_null < 2))}));  ) { (function () { if ( f_bool((v_last_pos == v_MATCH.f_to())) ) { (function () { (v_last_match_null = f_add(v_last_match_null, 1)); })() } else { (function () { (v_last_match_null = 0); })() };(v_last_pos = v_MATCH.f_to());(v_count = f_add(v_count, 1)); })() };(v_MATCH.v_to = v_last_pos);return((v_count > 0)) })()))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_ws;  // v8 bug workaround
  // method opt_ws
  Perlito$Grammar.f_opt_ws = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((((function () { var v_last_pos = null;
(v_last_pos = v_MATCH.f_to());if ( f_bool(( f_bool(((function () { return(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())) })())) ? false : true)) ) { (function () { (v_MATCH.v_to = v_last_pos); })() };return(1) })()))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_opt_ws;  // v8 bug workaround
  // method opt_ws2
  Perlito$Grammar.f_opt_ws2 = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((((function () { var v_last_pos = null;
(v_last_pos = v_MATCH.f_to());if ( f_bool(( f_bool(((function () { return(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())) })())) ? false : true)) ) { (function () { (v_MATCH.v_to = v_last_pos); })() };return(1) })()))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_opt_ws2;  // v8 bug workaround
  // method opt_ws3
  Perlito$Grammar.f_opt_ws3 = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((((function () { var v_last_pos = null;
(v_last_pos = v_MATCH.f_to());if ( f_bool(( f_bool(((function () { return(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())) })())) ? false : true)) ) { (function () { (v_MATCH.v_to = v_last_pos); })() };return(1) })()))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_opt_ws3;  // v8 bug workaround
  // method grammar
  Perlito$Grammar.f_grammar = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and(f_and(f_and(f_and(f_and(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_full_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["full_ident"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_last_pos = null;
(v_last_pos = v_MATCH.f_to());if ( f_bool(( f_bool(((function () { return(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())) })())) ? false : true)) ) { (function () { (v_MATCH.v_to = v_last_pos); })() };return(1) })())}), function () { return (f_and(("{" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return ((function () { var v_last_pos = null;
(v_last_pos = v_MATCH.f_to());if ( f_bool(( f_bool(((function () { return(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())) })())) ? false : true)) ) { (function () { (v_MATCH.v_to = v_last_pos); })() };return(1) })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_exp_stmts(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp_stmts"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_last_pos = null;
(v_last_pos = v_MATCH.f_to());if ( f_bool(( f_bool(((function () { return(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())) })())) ? false : true)) ) { (function () { (v_MATCH.v_to = v_last_pos); })() };return(1) })())}), function () { return (f_and(("}" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_name: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["full_ident"] ); })()),v_body: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp_stmts"] ); })())}; tmp.__proto__ = CompUnit; return tmp })())) })()), function () { return 1}))}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_grammar;  // v8 bug workaround
  // method declarator
  Perlito$Grammar.f_declarator = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(((function () { return(((f_and(("my" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))})))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("state" == (v_str || "").substr(v_MATCH.f_to(), 5)), function () { return ((v_MATCH.v_to = f_add(5, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("has" == (v_str || "").substr(v_MATCH.f_to(), 3)), function () { return ((v_MATCH.v_to = f_add(3, v_MATCH.f_to())))})))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_declarator;  // v8 bug workaround
  // method exp_stmts2
  Perlito$Grammar.f_exp_stmts2 = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_exp_stmts(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp_stmts"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp_stmts"] ); })()))) })()), function () { return 1}))}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_exp_stmts2;  // v8 bug workaround
  // method exp
  Perlito$Grammar.f_exp = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Expression.f_exp_parse(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Expression.exp_parse"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Expression.exp_parse"] ); })()))) })()), function () { return 1}))}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_exp;  // v8 bug workaround
  // method exp2
  Perlito$Grammar.f_exp2 = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Expression.f_exp_parse(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Expression.exp_parse"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Expression.exp_parse"] ); })()))) })()), function () { return 1}))}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_exp2;  // v8 bug workaround
  // method opt_ident
  Perlito$Grammar.f_opt_ident = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["ident"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["ident"] ); })()))) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(1, function () { return (f_or(((function () { return((v_MATCH.v_capture = "postcircumfix:<( )>")) })()), function () { return 1}))}))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_opt_ident;  // v8 bug workaround
  // method opt_type
  Perlito$Grammar.f_opt_type = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(f_and(((function () { var v_last_pos = null;
(v_last_pos = v_MATCH.f_to());if ( f_bool(( f_bool(((function () { return((f_and(("::" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))}))) })())) ? false : true)) ) { (function () { (v_MATCH.v_to = v_last_pos); })() };return(1) })()), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_full_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["full_ident"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["full_ident"] ); })()))) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(1, function () { return (f_or(((function () { return((v_MATCH.v_capture = "")) })()), function () { return 1}))}))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_opt_type;  // v8 bug workaround
  // method var_sigil
  Perlito$Grammar.f_var_sigil = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(f_or(((function () { return(((f_and(("$" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("%" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("@" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("&" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_var_sigil;  // v8 bug workaround
  // method var_twigil
  Perlito$Grammar.f_var_twigil = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((((function () { var v_last_pos = null;
(v_last_pos = v_MATCH.f_to());if ( f_bool(( f_bool(((function () { return(((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(f_or(((function () { return(((f_and(("." == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("!" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("^" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("*" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())})) })())) })())) ? false : true)) ) { (function () { (v_MATCH.v_to = v_last_pos); })() };return(1) })()))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_var_twigil;  // v8 bug workaround
  // method var_name
  Perlito$Grammar.f_var_name = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(((function () { return((((function () { var v_m2 = null;
(v_m2 = v_grammar.f_full_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["full_ident"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("/" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((((function () { var v_m2 = null;
(v_m2 = v_grammar.f_digit(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["digit"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_var_name;  // v8 bug workaround
  // method var_ident
  Perlito$Grammar.f_var_ident = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and(f_and(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_var_sigil(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["var_sigil"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_var_twigil(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["var_twigil"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_optional_namespace_before_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["optional_namespace_before_ident"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_var_name(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["var_name"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_sigil: ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["var_sigil"] ); })()).f_string(),v_twigil: ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["var_twigil"] ); })()).f_string(),v_namespace: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["optional_namespace_before_ident"] ); })()),v_name: ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["var_name"] ); })()).f_string()}; tmp.__proto__ = Var; return tmp })())) })()), function () { return 1}))}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_var_ident;  // v8 bug workaround
  // method exponent
  Perlito$Grammar.f_exponent = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and(((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return(((f_and(("e" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("E" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())})) })()), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(((function () { return(((f_and(("+" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("-" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((1)) })())})) })())}), function () { return ((function () { var v_last_match_null = null;
var v_last_pos = null;
var v_count = null;
(v_last_match_null = 0);(v_last_pos = v_MATCH.f_to());(v_count = 0);for ( ; f_bool(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_digit(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((v_last_match_null < 2))}));  ) { (function () { if ( f_bool((v_last_pos == v_MATCH.f_to())) ) { (function () { (v_last_match_null = f_add(v_last_match_null, 1)); })() } else { (function () { (v_last_match_null = 0); })() };(v_last_pos = v_MATCH.f_to());(v_count = f_add(v_count, 1)); })() };(v_MATCH.v_to = v_last_pos);return((v_count > 0)) })())}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_exponent;  // v8 bug workaround
  // method val_num
  Perlito$Grammar.f_val_num = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(f_and((f_and(("." == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_last_match_null = null;
var v_last_pos = null;
var v_count = null;
(v_last_match_null = 0);(v_last_pos = v_MATCH.f_to());(v_count = 0);for ( ; f_bool(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_digit(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((v_last_match_null < 2))}));  ) { (function () { if ( f_bool((v_last_pos == v_MATCH.f_to())) ) { (function () { (v_last_match_null = f_add(v_last_match_null, 1)); })() } else { (function () { (v_last_match_null = 0); })() };(v_last_pos = v_MATCH.f_to());(v_count = f_add(v_count, 1)); })() };(v_MATCH.v_to = v_last_pos);return((v_count > 0)) })())}), function () { return ((function () { var v_last_pos = null;
(v_last_pos = v_MATCH.f_to());if ( f_bool(( f_bool(((function () { return(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_exponent(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())) })())) ? false : true)) ) { (function () { (v_MATCH.v_to = v_last_pos); })() };return(1) })())}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_last_match_null = null;
var v_last_pos = null;
var v_count = null;
(v_last_match_null = 0);(v_last_pos = v_MATCH.f_to());(v_count = 0);for ( ; f_bool(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_digit(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((v_last_match_null < 2))}));  ) { (function () { if ( f_bool((v_last_pos == v_MATCH.f_to())) ) { (function () { (v_last_match_null = f_add(v_last_match_null, 1)); })() } else { (function () { (v_last_match_null = 0); })() };(v_last_pos = v_MATCH.f_to());(v_count = f_add(v_count, 1)); })() };(v_MATCH.v_to = v_last_pos);return((v_count > 0)) })()), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((((function () { var v_m2 = null;
(v_m2 = v_grammar.f_exponent(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and((f_and(("." == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_last_match_null = null;
var v_last_pos = null;
var v_count = null;
(v_last_match_null = 0);(v_last_pos = v_MATCH.f_to());(v_count = 0);for ( ; f_bool(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_digit(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((v_last_match_null < 2))}));  ) { (function () { if ( f_bool((v_last_pos == v_MATCH.f_to())) ) { (function () { (v_last_match_null = f_add(v_last_match_null, 1)); })() } else { (function () { (v_last_match_null = 0); })() };(v_last_pos = v_MATCH.f_to());(v_count = f_add(v_count, 1)); })() };(v_MATCH.v_to = v_last_pos);return((v_count > 0)) })())}), function () { return ((function () { var v_last_pos = null;
(v_last_pos = v_MATCH.f_to());if ( f_bool(( f_bool(((function () { return(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_exponent(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())) })())) ? false : true)) ) { (function () { (v_MATCH.v_to = v_last_pos); })() };return(1) })())}))) })())})) })())}))) })())})) })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_num: (v_MATCH).f_string()}; tmp.__proto__ = Val$Num; return tmp })())) })()), function () { return 1}))}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_val_num;  // v8 bug workaround
  // method char_any
  Perlito$Grammar.f_char_any = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return(((f_and(("" != (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_char_any;  // v8 bug workaround
  // method char_any_single_quote
  Perlito$Grammar.f_char_any_single_quote = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and(((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return(((f_and(("'" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? false : true));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })()), function () { return (f_and(("" != (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return ((function () { var v_last_match_null = null;
var v_last_pos = null;
(v_last_match_null = 0);(v_last_pos = v_MATCH.f_to());for ( ; f_bool(f_and(((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return(((f_and(("'" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("\\" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())})) })()))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? false : true));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })()), function () { return (f_and(("" != (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}))) })())) })()), function () { return ((v_last_match_null < 2))}));  ) { (function () { if ( f_bool((v_last_pos == v_MATCH.f_to())) ) { (function () { (v_last_match_null = f_add(v_last_match_null, 1)); })() } else { (function () { (v_last_match_null = 0); })() };(v_last_pos = v_MATCH.f_to()); })() };(v_MATCH.v_to = v_last_pos);return(1) })())}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_char_any_single_quote;  // v8 bug workaround
  // method single_quoted_unescape
  Perlito$Grammar.f_single_quoted_unescape = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(f_or(f_or(((function () { return((f_and(f_and(f_and((f_and(("\\" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return (f_and(("\\" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_single_quoted_unescape(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["single_quoted_unescape"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (f_string("\\") + f_string((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["single_quoted_unescape"] ); })())))) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("\\" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return (f_and(("'" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_single_quoted_unescape(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["single_quoted_unescape"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (f_string("'") + f_string((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["single_quoted_unescape"] ); })())))) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and((f_and(("\\" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_single_quoted_unescape(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["single_quoted_unescape"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (f_string("\\") + f_string((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["single_quoted_unescape"] ); })())))) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_char_any_single_quote(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["char_any_single_quote"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_single_quoted_unescape(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["single_quoted_unescape"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (f_string((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["char_any_single_quote"] ); })()) + f_string((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["single_quoted_unescape"] ); })())))) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((1)) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_single_quoted_unescape;  // v8 bug workaround
  // method char_any_double_quote
  Perlito$Grammar.f_char_any_double_quote = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and(((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(f_or(f_or(((function () { return(((f_and(("\"" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("$" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("@" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("%" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("{" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())})) })()))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? false : true));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })()), function () { return (f_and(("" != (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return ((function () { var v_last_match_null = null;
var v_last_pos = null;
(v_last_match_null = 0);(v_last_pos = v_MATCH.f_to());for ( ; f_bool(f_and(((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(f_or(f_or(f_or(((function () { return(((f_and(("\"" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("$" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("@" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("%" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("{" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("\\" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())})) })()))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? false : true));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })()), function () { return (f_and(("" != (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}))) })())) })()), function () { return ((v_last_match_null < 2))}));  ) { (function () { if ( f_bool((v_last_pos == v_MATCH.f_to())) ) { (function () { (v_last_match_null = f_add(v_last_match_null, 1)); })() } else { (function () { (v_last_match_null = 0); })() };(v_last_pos = v_MATCH.f_to()); })() };(v_MATCH.v_to = v_last_pos);return(1) })())}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_char_any_double_quote;  // v8 bug workaround
  // method double_quoted_unescape
  Perlito$Grammar.f_double_quoted_unescape = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(f_or(((function () { return((f_and(f_and((f_and(("\\" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return (f_and(("n" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (f_string("\n")))) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and((f_and(("\\" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return (f_and(("t" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = f_chr(9))) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and((f_and(("\\" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_char_any(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["char_any"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["char_any"] ); })()).f_string())) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_char_any_double_quote(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["char_any_double_quote"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["char_any_double_quote"] ); })()).f_string())) })()), function () { return 1}))}))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_double_quoted_unescape;  // v8 bug workaround
  // method double_quoted_buf
  Perlito$Grammar.f_double_quoted_buf = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(f_or(f_or(((function () { return((f_and(((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return(((f_and(("$" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? true : false));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })()), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(f_and(((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and((f_and(("$" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_var_twigil(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? true : false));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })()), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Expression.f_operator(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Expression.operator"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Expression.operator"] ); })()))[1])) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_char_any(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["char_any"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_buf: ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["char_any"] ); })()).f_string()}; tmp.__proto__ = Val$Buf; return tmp })())) })()), function () { return 1}))}))) })())})) })())}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return(((f_and(("@" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? true : false));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })()), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(f_and(f_and(((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and((f_and(("@" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_var_twigil(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? true : false));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })()), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Expression.f_operator(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Expression.operator"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("[]" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Expression.operator"] ); })()))[1])) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_char_any(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["char_any"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_buf: ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["char_any"] ); })()).f_string()}; tmp.__proto__ = Val$Buf; return tmp })())) })()), function () { return 1}))}))) })())})) })())}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return(((f_and(("%" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? true : false));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })()), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(f_and(f_and(((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and((f_and(("%" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_var_twigil(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? true : false));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })()), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Expression.f_operator(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Expression.operator"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("{}" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Expression.operator"] ); })()))[1])) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_char_any(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["char_any"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_buf: ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["char_any"] ); })()).f_string()}; tmp.__proto__ = Val$Buf; return tmp })())) })()), function () { return 1}))}))) })())})) })())}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("{" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_exp_stmts(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp_stmts"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("}" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_block: (function () { var tmp = {v_stmts: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp_stmts"] ); })())}; tmp.__proto__ = Lit$Block; return tmp })()}; tmp.__proto__ = Do; return tmp })())) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_double_quoted_unescape(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["double_quoted_unescape"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_buf: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["double_quoted_unescape"] ); })())}; tmp.__proto__ = Val$Buf; return tmp })())) })()), function () { return 1}))}))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_double_quoted_buf;  // v8 bug workaround
  // method val_buf
  Perlito$Grammar.f_val_buf = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(f_and(f_and((f_and(("\"" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_last_match_null = null;
var v_last_pos = null;
(v_last_match_null = 0);(v_last_pos = v_MATCH.f_to());for ( ; f_bool(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_double_quoted_buf(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());if ( f_bool((v_MATCH).hasOwnProperty("double_quoted_buf")) ) { (function () { ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["double_quoted_buf"] ); })()).push(v_m2); })() } else { (function () { (function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["double_quoted_buf"]  = [v_m2]); })(); })() };return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((v_last_match_null < 2))}));  ) { (function () { if ( f_bool((v_last_pos == v_MATCH.f_to())) ) { (function () { (v_last_match_null = f_add(v_last_match_null, 1)); })() } else { (function () { (v_last_match_null = 0); })() };(v_last_pos = v_MATCH.f_to()); })() };(v_MATCH.v_to = v_last_pos);return(1) })())}), function () { return (f_and(("\"" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { var v_args = null;
(v_args = (function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["double_quoted_buf"] ); })());if ( f_bool(( f_bool(v_args) ? false : true)) ) { return (function () { return((v_MATCH.v_capture = (function () { var tmp = {v_buf: ""}; tmp.__proto__ = Val$Buf; return tmp })())) })() } else { return (function () { return((v_MATCH.v_capture = (function () { var tmp = {v_namespace: "",v_code: "list:<~>",v_arguments: (function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_capture() ) }; return out; })(((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["double_quoted_buf"] ); })()))}; tmp.__proto__ = Apply; return tmp })())) })() } })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("'" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_single_quoted_unescape(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["single_quoted_unescape"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("'" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_buf: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["single_quoted_unescape"] ); })())}; tmp.__proto__ = Val$Buf; return tmp })())) })()), function () { return 1}))}))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_val_buf;  // v8 bug workaround
  // method val_int
  Perlito$Grammar.f_val_int = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(((function () { var v_last_match_null = null;
var v_last_pos = null;
var v_count = null;
(v_last_match_null = 0);(v_last_pos = v_MATCH.f_to());(v_count = 0);for ( ; f_bool(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_digit(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((v_last_match_null < 2))}));  ) { (function () { if ( f_bool((v_last_pos == v_MATCH.f_to())) ) { (function () { (v_last_match_null = f_add(v_last_match_null, 1)); })() } else { (function () { (v_last_match_null = 0); })() };(v_last_pos = v_MATCH.f_to());(v_count = f_add(v_count, 1)); })() };(v_MATCH.v_to = v_last_pos);return((v_count > 0)) })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_int: (v_MATCH).f_string()}; tmp.__proto__ = Val$Int; return tmp })())) })()), function () { return 1}))}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_val_int;  // v8 bug workaround
  // method exp_stmts
  Perlito$Grammar.f_exp_stmts = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Expression.f_statement_parse(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Expression.statement_parse"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(f_and(f_and(f_and(f_and(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return(((f_and((";" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((1)) })())})) })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_exp_stmts(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp_stmts"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and((f_and((";" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((1)) })())})) })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var a = []; a.push(f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Expression.statement_parse"] ); })())); (function(a_) { for (var i_ = 0; i_ < a_.length ; i_++) { a.push(a_[i_]) }})(((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp_stmts"] ); })()))));  return a })())) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and((f_and((";" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((1)) })())})) })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = [f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Expression.statement_parse"] ); })())])) })()), function () { return 1}))}))) })())})) })())}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_or(((function () { return((v_MATCH.v_capture = [])) })()), function () { return 1})))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_exp_stmts;  // v8 bug workaround
  // method opt_name
  Perlito$Grammar.f_opt_name = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((((function () { var v_last_pos = null;
(v_last_pos = v_MATCH.f_to());if ( f_bool(( f_bool(((function () { return(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());if ( f_bool((v_MATCH).hasOwnProperty("ident")) ) { (function () { ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["ident"] ); })()).push(v_m2); })() } else { (function () { (function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["ident"]  = [v_m2]); })(); })() };return(1) })() } else { return (function () { return(false) })() } })())) })())) ? false : true)) ) { (function () { (v_MATCH.v_to = v_last_pos); })() };return(1) })()))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_opt_name;  // v8 bug workaround
  // method var_invocant
  Perlito$Grammar.f_var_invocant = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_var_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["var_ident"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_and((":" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["var_ident"] ); })()))) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_sigil: "$",v_twigil: "",v_name: "self"}; tmp.__proto__ = Var; return tmp })())) })()), function () { return 1})))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_var_invocant;  // v8 bug workaround
  // method args_sig
  Perlito$Grammar.f_args_sig = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_var_invocant(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["var_invocant"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Expression.f_list_parse(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Expression.list_parse"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_invocant: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["var_invocant"] ); })()),v_positional: Perlito$Expression.f_expand_list((function () { if ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Expression.list_parse"] ); })())) == null) { (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Expression.list_parse"] ); })())) = {} }; return ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Expression.list_parse"] ); })()))["exp"] ); })()),v_named: (function () { var a = {}; ; return a })()}; tmp.__proto__ = Sig; return tmp })())) })()), function () { return 1}))}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_args_sig;  // v8 bug workaround
  // method method_sig
  Perlito$Grammar.f_method_sig = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(f_and(f_and(f_and(f_and(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_and(("(" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_args_sig(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["args_sig"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and((")" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["args_sig"] ); })()))) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_invocant: (function () { var tmp = {v_sigil: "$",v_twigil: "",v_name: "self"}; tmp.__proto__ = Var; return tmp })(),v_positional: [],v_named: (function () { var a = {}; ; return a })()}; tmp.__proto__ = Sig; return tmp })())) })()), function () { return 1})))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_method_sig;  // v8 bug workaround
  // method method_def
  Perlito$Grammar.f_method_def = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and(f_and(f_and(f_and(f_and(f_and(f_and(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_name(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["opt_name"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_method_sig(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["method_sig"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("{" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_exp_stmts(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp_stmts"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return(((f_and(("}" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_or(((function () { return(f_die("Syntax Error in method '.", f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["name"] ); })()), "' near pos=", v_MATCH.f_to())) })()), function () { return 1})))) })())})) })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_name: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["opt_name"] ); })()),v_sig: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["method_sig"] ); })()),v_block: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp_stmts"] ); })())}; tmp.__proto__ = Method; return tmp })())) })()), function () { return 1}))}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_method_def;  // v8 bug workaround
  // method sub_def
  Perlito$Grammar.f_sub_def = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and(f_and(f_and(f_and(f_and(f_and(f_and(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_name(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["opt_name"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_method_sig(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["method_sig"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("{" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_exp_stmts(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp_stmts"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return(((f_and(("}" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_or(((function () { return(f_die("Syntax Error in sub '", f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["name"] ); })()), "'")) })()), function () { return 1})))) })())})) })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_name: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["opt_name"] ); })()),v_sig: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["method_sig"] ); })()),v_block: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp_stmts"] ); })())}; tmp.__proto__ = Sub; return tmp })())) })()), function () { return 1}))}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_sub_def;  // v8 bug workaround
  // method token
  Perlito$Grammar.f_token = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and(f_and(f_and(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_name(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["opt_name"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("{" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar$Regex.f_rule(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar::Regex.rule"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("}" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { var v_source = null;
var v_ast = null;
(v_source = (f_string((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["opt_name"] ); })()) + f_string(" ( $grammar: $str, $pos ) { ") + f_string("my $MATCH; $MATCH = Perlito::Match.new( str => $str, from => $pos, to => $pos, bool => 1 ); ") + f_string("$MATCH.bool = ( ") + f_string((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar::Regex.rule"] ); })())).f_emit_perl6()) + f_string("); ") + f_string("$MATCH }")));(v_ast = Perlito$Grammar.f_method_def(v_source, 0));return((v_MATCH.v_capture = f_scalar(v_ast))) })()), function () { return 1}))}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_token;  // v8 bug workaround
// use Perlito::Expression
;// use Perlito::Grammar::Regex
;// use Perlito::Grammar::Control
;})();
;})();

// class GLOBAL
if (typeof GLOBAL != 'object') {
  GLOBAL = function() {};
  GLOBAL = new GLOBAL;
  GLOBAL.f_isa = function (s) { return s == 'GLOBAL' };
  GLOBAL.f_perl = function () { return 'GLOBAL.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = GLOBAL;
// use v6
;// class Perlito::Grammar
if (typeof Perlito$Grammar != 'object') {
  Perlito$Grammar = function() {};
  Perlito$Grammar = new Perlito$Grammar;
  Perlito$Grammar.f_isa = function (s) { return s == 'Perlito::Grammar' };
  Perlito$Grammar.f_perl = function () { return 'Perlito::Grammar.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Perlito$Grammar;
  // method unless
  Perlito$Grammar.f_unless = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and(f_and(f_and(f_and(f_and(f_and(f_and((f_and(("u" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return (f_and(("n" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_and(("l" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_and(("e" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_and(("s" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_and(("s" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_exp(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { var v_body = null;
(v_body = (function () { if ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) == null) { (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) = {} }; return ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })()))["end_block"] ); })());if ( f_bool(( f_bool(((v_body != null))) ? false : true)) ) { (function () { f_die((f_string("Missing code block in 'unless'"))); })() };return((v_MATCH.v_capture = (function () { var tmp = {v_cond: (function () { if ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) == null) { (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) = {} }; return ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })()))["exp"] ); })(),v_body: (function () { var tmp = {v_stmts: []}; tmp.__proto__ = Lit$Block; return tmp })(),v_otherwise: v_body}; tmp.__proto__ = If; return tmp })())) })()), function () { return 1}))}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_unless;  // v8 bug workaround
  // method if
  Perlito$Grammar.f_if = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and(f_and(f_and((f_and(("i" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return (f_and(("f" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_exp(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(((function () { return((f_and(f_and(f_and(f_and(f_and(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_and(("e" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_and(("l" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_and(("s" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_and(("e" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_exp2(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp2"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { var v_body = null;
var v_otherwise = null;
(v_body = (function () { if ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) == null) { (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) = {} }; return ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })()))["end_block"] ); })());(v_otherwise = (function () { if ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp2"] ); })())) == null) { (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp2"] ); })())) = {} }; return ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp2"] ); })()))["exp"] ); })());if ( f_bool(( f_bool(((v_body != null))) ? false : true)) ) { (function () { f_die((f_string("Missing code block in 'if'"))); })() };if ( f_bool(( f_bool(((v_otherwise != null))) ? false : true)) ) { (function () { f_die((f_string("Missing code block in 'else'"))); })() };if ( f_bool(f_isa(v_otherwise, "Lit::Hash")) ) { (function () { (v_otherwise = (function () { var tmp = {v_stmts: v_otherwise.f_hash1()}; tmp.__proto__ = Lit$Block; return tmp })()); })() };return((v_MATCH.v_capture = (function () { var tmp = {v_cond: (function () { if ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) == null) { (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) = {} }; return ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })()))["exp"] ); })(),v_body: v_body,v_otherwise: v_otherwise}; tmp.__proto__ = If; return tmp })())) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and(f_and(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_and(("e" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_and(("l" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_and(("s" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_if(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["if"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { var v_body = null;
(v_body = (function () { if ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) == null) { (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) = {} }; return ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })()))["end_block"] ); })());if ( f_bool(( f_bool(((v_body != null))) ? false : true)) ) { (function () { f_die((f_string("Missing code block in 'if'"))); })() };return((v_MATCH.v_capture = (function () { var tmp = {v_cond: (function () { if ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) == null) { (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) = {} }; return ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })()))["exp"] ); })(),v_body: v_body,v_otherwise: (function () { var tmp = {v_stmts: [f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["if"] ); })())]}; tmp.__proto__ = Lit$Block; return tmp })()}; tmp.__proto__ = If; return tmp })())) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_or(((function () { var v_body = null;
(v_body = (function () { if ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) == null) { (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) = {} }; return ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })()))["end_block"] ); })());if ( f_bool(( f_bool(((v_body != null))) ? false : true)) ) { (function () { f_die((f_string("Missing code block in 'if'"))); })() };return((v_MATCH.v_capture = (function () { var tmp = {v_cond: (function () { if ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) == null) { (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) = {} }; return ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })()))["exp"] ); })(),v_body: v_body,v_otherwise: []}; tmp.__proto__ = If; return tmp })())) })()), function () { return 1})))) })())})) })())}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_if;  // v8 bug workaround
  // method when
  Perlito$Grammar.f_when = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and(f_and(f_and(f_and(f_and((f_and(("w" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return (f_and(("h" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_and(("e" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_and(("n" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_exp(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { var v_body = null;
(v_body = (function () { if ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) == null) { (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) = {} }; return ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })()))["end_block"] ); })());if ( f_bool(( f_bool(((v_body != null))) ? false : true)) ) { (function () { f_die((f_string("Missing code block in 'when'"))); })() };return((v_MATCH.v_capture = (function () { var tmp = {v_parameters: (function () { if ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) == null) { (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) = {} }; return ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })()))["exp"] ); })(),v_body: v_body}; tmp.__proto__ = When; return tmp })())) })()), function () { return 1}))}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_when;  // v8 bug workaround
  // method for
  Perlito$Grammar.f_for = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and(f_and(f_and(f_and((f_and(("f" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return (f_and(("o" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_and(("r" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_exp(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { var v_body = null;
(v_body = (function () { if ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) == null) { (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) = {} }; return ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })()))["end_block"] ); })());if ( f_bool(( f_bool(((v_body != null))) ? false : true)) ) { (function () { f_die((f_string("Missing code block in 'when'"))); })() };return((v_MATCH.v_capture = (function () { var tmp = {v_cond: (function () { if ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) == null) { (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) = {} }; return ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })()))["exp"] ); })(),v_topic: null,v_body: v_body}; tmp.__proto__ = For; return tmp })())) })()), function () { return 1}))}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_for;  // v8 bug workaround
  // method while
  Perlito$Grammar.f_while = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and(f_and(f_and(f_and(f_and(f_and((f_and(("w" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return (f_and(("h" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_and(("i" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_and(("l" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_and(("e" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_exp(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { var v_body = null;
(v_body = (function () { if ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) == null) { (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) = {} }; return ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })()))["end_block"] ); })());if ( f_bool(( f_bool(((v_body != null))) ? false : true)) ) { (function () { f_die((f_string("Missing code block in 'while'"))); })() };return((v_MATCH.v_capture = (function () { var tmp = {v_cond: (function () { if ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) == null) { (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) = {} }; return ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })()))["exp"] ); })(),v_body: v_body}; tmp.__proto__ = While; return tmp })())) })()), function () { return 1}))}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_while;  // v8 bug workaround
  // method loop
  Perlito$Grammar.f_loop = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and(f_and(f_and(f_and(f_and((f_and(("l" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return (f_and(("o" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_and(("o" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_and(("p" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_exp(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { var v_body = null;
(v_body = (function () { if ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) == null) { (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) = {} }; return ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })()))["end_block"] ); })());if ( f_bool(( f_bool(((v_body != null))) ? false : true)) ) { return (function () { (v_body = (function () { if ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) == null) { (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })())) = {} }; return ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["exp"] ); })()))["exp"] ); })());if ( f_bool(f_isa(v_body, "Lit::Block")) ) { return (function () { return((v_MATCH.v_capture = (function () { var tmp = {v_cond: (function () { var tmp = {v_bit: 1}; tmp.__proto__ = Val$Bit; return tmp })(),v_body: v_body}; tmp.__proto__ = While; return tmp })())) })() } else { return (function () { return(f_die((f_string("Missing code block in 'loop'")))) })() } })() } else { return (function () { return(f_die((f_string("'loop' with parameters is not implemented")))) })() } })()), function () { return 1}))}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar.f_loop;  // v8 bug workaround
})();
;})();

// class GLOBAL
if (typeof GLOBAL != 'object') {
  GLOBAL = function() {};
  GLOBAL = new GLOBAL;
  GLOBAL.f_isa = function (s) { return s == 'GLOBAL' };
  GLOBAL.f_perl = function () { return 'GLOBAL.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = GLOBAL;
// use v6
;// class Perlito::Grammar::Regex
if (typeof Perlito$Grammar$Regex != 'object') {
  Perlito$Grammar$Regex = function() {};
  Perlito$Grammar$Regex = new Perlito$Grammar$Regex;
  Perlito$Grammar$Regex.f_isa = function (s) { return s == 'Perlito::Grammar::Regex' };
  Perlito$Grammar$Regex.f_perl = function () { return 'Perlito::Grammar::Regex.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Perlito$Grammar$Regex;
var Hash_rule_terms = {};
  // method ws
  Perlito$Grammar$Regex.f_ws = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar$Regex.f_ws;  // v8 bug workaround
  // method rule_ident
  Perlito$Grammar$Regex.f_rule_ident = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_full_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((((function () { var v_m2 = null;
(v_m2 = v_grammar.f_digit(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["digit"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar$Regex.f_rule_ident;  // v8 bug workaround
  // method any
  Perlito$Grammar$Regex.f_any = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return(((f_and(("" != (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar$Regex.f_any;  // v8 bug workaround
  // method literal
  Perlito$Grammar$Regex.f_literal = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((((function () { var v_last_match_null = null;
var v_last_pos = null;
(v_last_match_null = 0);(v_last_pos = v_MATCH.f_to());for ( ; f_bool(f_and(((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and((f_and(("\\" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return (f_and(("" != (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return(((f_and(("'" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? false : true));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })()), function () { return (f_and(("" != (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}))) })())})) })()), function () { return ((v_last_match_null < 2))}));  ) { (function () { if ( f_bool((v_last_pos == v_MATCH.f_to())) ) { (function () { (v_last_match_null = f_add(v_last_match_null, 1)); })() } else { (function () { (v_last_match_null = 0); })() };(v_last_pos = v_MATCH.f_to()); })() };(v_MATCH.v_to = v_last_pos);return(1) })()))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar$Regex.f_literal;  // v8 bug workaround
  // method metasyntax_exp
  Perlito$Grammar$Regex.f_metasyntax_exp = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((((function () { var v_last_match_null = null;
var v_last_pos = null;
var v_count = null;
(v_last_match_null = 0);(v_last_pos = v_MATCH.f_to());(v_count = 0);for ( ; f_bool(f_and(((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(f_or(f_or(((function () { return((f_and((f_and(("\\" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return (f_and(("" != (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and((f_and(("'" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_literal(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("'" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and((f_and(("{" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_string_code(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("}" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and((f_and(("<" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_metasyntax_exp(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and((">" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return(((f_and((">" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? false : true));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })()), function () { return (f_and(("" != (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}))) })())})) })()), function () { return ((v_last_match_null < 2))}));  ) { (function () { if ( f_bool((v_last_pos == v_MATCH.f_to())) ) { (function () { (v_last_match_null = f_add(v_last_match_null, 1)); })() } else { (function () { (v_last_match_null = 0); })() };(v_last_pos = v_MATCH.f_to());(v_count = f_add(v_count, 1)); })() };(v_MATCH.v_to = v_last_pos);return((v_count > 0)) })()))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar$Regex.f_metasyntax_exp;  // v8 bug workaround
  // method char_range
  Perlito$Grammar$Regex.f_char_range = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((((function () { var v_last_match_null = null;
var v_last_pos = null;
var v_count = null;
(v_last_match_null = 0);(v_last_pos = v_MATCH.f_to());(v_count = 0);for ( ; f_bool(f_and(((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and((f_and(("\\" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return (f_and(("" != (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return(((f_and(("]" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? false : true));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })()), function () { return (f_and(("" != (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}))) })())})) })()), function () { return ((v_last_match_null < 2))}));  ) { (function () { if ( f_bool((v_last_pos == v_MATCH.f_to())) ) { (function () { (v_last_match_null = f_add(v_last_match_null, 1)); })() } else { (function () { (v_last_match_null = 0); })() };(v_last_pos = v_MATCH.f_to());(v_count = f_add(v_count, 1)); })() };(v_MATCH.v_to = v_last_pos);return((v_count > 0)) })()))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar$Regex.f_char_range;  // v8 bug workaround
  // method char_class
  Perlito$Grammar$Regex.f_char_class = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((((function () { var v_m2 = null;
(v_m2 = v_grammar.f_rule_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and((f_and(("[" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_char_range(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("]" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar$Regex.f_char_class;  // v8 bug workaround
  // method string_code
  Perlito$Grammar$Regex.f_string_code = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((((function () { var v_last_match_null = null;
var v_last_pos = null;
var v_count = null;
(v_last_match_null = 0);(v_last_pos = v_MATCH.f_to());(v_count = 0);for ( ; f_bool(f_and(((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(f_or(((function () { return((f_and((f_and(("\\" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return (f_and(("" != (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and((f_and(("'" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_literal(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("'" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and((f_and(("{" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_string_code(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("}" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return(((f_and(("}" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? false : true));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })()), function () { return (f_and(("" != (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}))) })())})) })()), function () { return ((v_last_match_null < 2))}));  ) { (function () { if ( f_bool((v_last_pos == v_MATCH.f_to())) ) { (function () { (v_last_match_null = f_add(v_last_match_null, 1)); })() } else { (function () { (v_last_match_null = 0); })() };(v_last_pos = v_MATCH.f_to());(v_count = f_add(v_count, 1)); })() };(v_MATCH.v_to = v_last_pos);return((v_count > 0)) })()))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar$Regex.f_string_code;  // v8 bug workaround
  // method parsed_code
  Perlito$Grammar$Regex.f_parsed_code = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_string_code(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = (v_MATCH).f_string())) })()), function () { return 1}))}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar$Regex.f_parsed_code;  // v8 bug workaround
  // method named_capture_body
  Perlito$Grammar$Regex.f_named_capture_body = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(f_or(((function () { return((f_and(f_and(f_and((f_and(("(" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_rule(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and((")" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var a = {}; a["capturing_group"] = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule"] ); })()); return a })())) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("[" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_rule(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("]" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule"] ); })()))) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("<" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_metasyntax_exp(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["metasyntax_exp"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and((">" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_metasyntax: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["metasyntax_exp"] ); })()),v_captures: 1}; tmp.__proto__ = Rul$Subrule; return tmp })())) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_or(((function () { return(f_die("invalid alias syntax")) })()), function () { return 1})))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar$Regex.f_named_capture_body;  // v8 bug workaround
  // method variables
  Perlito$Grammar$Regex.f_variables = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(((function () { return((f_and(f_and(f_and((f_and(("$<" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_rule_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule_ident"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and((">" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (f_string("$/{") + f_string("'") + f_string((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule_ident"] ); })()) + f_string("'") + f_string("}")))) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_var_sigil(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.var_sigil"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_val_int(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.val_int"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (f_string((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.var_sigil"] ); })()) + f_string("/[") + f_string((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.val_int"] ); })()) + f_string("]")))) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_var_sigil(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.var_sigil"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_var_twigil(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.var_twigil"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_full_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.full_ident"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_sigil: ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.var_sigil"] ); })()).f_string(),v_twigil: ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.var_twigil"] ); })()).f_string(),v_name: ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.full_ident"] ); })()).f_string()}; tmp.__proto__ = Rul$Var; return tmp })())) })()), function () { return 1}))}))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar$Regex.f_variables;  // v8 bug workaround
  // method rule_terms
  Perlito$Grammar$Regex.f_rule_terms = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(((function () { return((f_and(f_and(f_and((f_and(("(" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_rule(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and((")" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_rule_exp: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule"] ); })())}; tmp.__proto__ = Rul$Capture; return tmp })())) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("<(" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_rule(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and((")>" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_rule_exp: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule"] ); })())}; tmp.__proto__ = Rul$CaptureResult; return tmp })())) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and(f_and((f_and(("<after" == (v_str || "").substr(v_MATCH.f_to(), 6)), function () { return ((v_MATCH.v_to = f_add(6, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_rule(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and((">" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_rule_exp: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule"] ); })())}; tmp.__proto__ = Rul$After; return tmp })())) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and(f_and((f_and(("<before" == (v_str || "").substr(v_MATCH.f_to(), 7)), function () { return ((v_MATCH.v_to = f_add(7, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_rule(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and((">" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_rule_exp: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule"] ); })())}; tmp.__proto__ = Rul$Before; return tmp })())) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and(f_and((f_and(("<!before" == (v_str || "").substr(v_MATCH.f_to(), 8)), function () { return ((v_MATCH.v_to = f_add(8, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_rule(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and((">" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_rule_exp: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule"] ); })())}; tmp.__proto__ = Rul$NotBefore; return tmp })())) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("<!" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_metasyntax_exp(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["metasyntax_exp"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and((">" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var a = {}; a["negate"] = (function () { var a = {}; a["metasyntax"] = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["metasyntax_exp"] ); })()); return a })(); return a })())) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("<+" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_char_class(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["char_class"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and((">" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_chars: ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["char_class"] ); })()).f_string()}; tmp.__proto__ = Rul$CharClass; return tmp })())) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("<-" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_char_class(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["char_class"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and((">" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_chars: ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["char_class"] ); })()).f_string()}; tmp.__proto__ = Rul$NegateCharClass; return tmp })())) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("'" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_literal(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["literal"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("'" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_constant: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["literal"] ); })())}; tmp.__proto__ = Rul$Constant; return tmp })())) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and(f_and(f_and((f_and(("<" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return (f_and(("'" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_literal(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["literal"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("'" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_and((">" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_constant: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["literal"] ); })())}; tmp.__proto__ = Rul$Constant; return tmp })())) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and((f_and(("<" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(f_or(((function () { return((f_and(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_variables(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["variables"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_and((">" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_var: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["variables"] ); })())}; tmp.__proto__ = Rul$InterpolateVar; return tmp })())) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("?" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_metasyntax_exp(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["metasyntax_exp"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and((">" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_metasyntax: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["metasyntax_exp"] ); })()),v_captures: 0}; tmp.__proto__ = Rul$Subrule; return tmp })())) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("." == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_metasyntax_exp(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["metasyntax_exp"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and((">" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_metasyntax: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["metasyntax_exp"] ); })()),v_captures: 0}; tmp.__proto__ = Rul$Subrule; return tmp })())) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_metasyntax_exp(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["metasyntax_exp"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_and((">" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_metasyntax: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["metasyntax_exp"] ); })()),v_captures: 1}; tmp.__proto__ = Rul$Subrule; return tmp })())) })()), function () { return 1}))}))) })())})) })())}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("{" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_parsed_code(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["parsed_code"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("}" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_closure: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["parsed_code"] ); })())}; tmp.__proto__ = Rul$Block; return tmp })())) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and((f_and(("\\" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_any(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["any"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_char: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["any"] ); })())}; tmp.__proto__ = Rul$SpecialChar; return tmp })())) })()), function () { return 1}))}))) })())) })())}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and((f_and(("." == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {}; tmp.__proto__ = Rul$Dot; return tmp })())) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("[" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_rule(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("]" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule"] ); })()))) })()), function () { return 1}))}))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar$Regex.f_rule_terms;  // v8 bug workaround
  // method rule_term
  Perlito$Grammar$Regex.f_rule_term = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(((function () { return((f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_variables(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["variables"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(f_and(f_and(f_and(((function () { var v_last_pos = null;
(v_last_pos = v_MATCH.f_to());if ( f_bool(( f_bool(((function () { return(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())) })())) ? false : true)) ) { (function () { (v_MATCH.v_to = v_last_pos); })() };return(1) })()), function () { return (f_and(("=" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return ((function () { var v_last_pos = null;
(v_last_pos = v_MATCH.f_to());if ( f_bool(( f_bool(((function () { return(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())) })())) ? false : true)) ) { (function () { (v_MATCH.v_to = v_last_pos); })() };return(1) })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_named_capture_body(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["named_capture_body"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_rule_exp: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["named_capture_body"] ); })()),v_capture_ident: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["variables"] ); })())}; tmp.__proto__ = Rul$NamedCapture; return tmp })())) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_or(((function () { return((v_MATCH.v_capture = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["variables"] ); })()))) })()), function () { return 1})))) })())})) })())}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_rule_terms(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule_terms"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule_terms"] ); })()))) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(((function () { return(((f_and(("]" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("}" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and((")" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and((">" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and((":" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("?" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("+" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("*" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("|" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("&" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("/" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())})) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? false : true));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })()), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_any(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["any"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_constant: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["any"] ); })())}; tmp.__proto__ = Rul$Constant; return tmp })())) })()), function () { return 1}))}))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar$Regex.f_rule_term;  // v8 bug workaround
  // method quant_exp
  Perlito$Grammar$Regex.f_quant_exp = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(f_and((f_and(("**" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_val_int(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.val_int"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.val_int"] ); })()))) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_rule_term(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule_term"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule_term"] ); })()))) })()), function () { return 1}))}))) })())})) })())}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(((function () { return(((f_and(("?" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("*" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("+" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())})) })()))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar$Regex.f_quant_exp;  // v8 bug workaround
  // method greedy_exp
  Perlito$Grammar$Regex.f_greedy_exp = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(((function () { return(((f_and(("?" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("+" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((1)) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar$Regex.f_greedy_exp;  // v8 bug workaround
  // method quantifier
  Perlito$Grammar$Regex.f_quantifier = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and(f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_rule_term(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule_term"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_opt_ws2(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(f_and(f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_quant_exp(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["quant_exp"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_greedy_exp(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["greedy_exp"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_opt_ws3(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_term: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule_term"] ); })()),v_quant: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["quant_exp"] ); })()),v_greedy: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["greedy_exp"] ); })()),v_ws1: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.opt_ws"] ); })()),v_ws2: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.opt_ws2"] ); })()),v_ws3: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.opt_ws3"] ); })())}; tmp.__proto__ = Rul$Quantifier; return tmp })())) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_or(((function () { return((v_MATCH.v_capture = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["rule_term"] ); })()))) })()), function () { return 1})))) })())})) })())}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar$Regex.f_quantifier;  // v8 bug workaround
  // method concat_list
  Perlito$Grammar$Regex.f_concat_list = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_quantifier(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["quantifier"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_concat_list(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["concat_list"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var a = []; a.push(f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["quantifier"] ); })())); (function(a_) { for (var i_ = 0; i_ < a_.length ; i_++) { a.push(a_[i_]) }})(((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["concat_list"] ); })()))));  return a })())) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_or(((function () { return((v_MATCH.v_capture = [f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["quantifier"] ); })())])) })()), function () { return 1})))) })())})) })())}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_or(((function () { return((v_MATCH.v_capture = [])) })()), function () { return 1})))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar$Regex.f_concat_list;  // v8 bug workaround
  // method concat_exp
  Perlito$Grammar$Regex.f_concat_exp = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_concat_list(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["concat_list"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_concat: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["concat_list"] ); })())}; tmp.__proto__ = Rul$Concat; return tmp })())) })()), function () { return 1}))}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar$Regex.f_concat_exp;  // v8 bug workaround
  // method or_list_exp
  Perlito$Grammar$Regex.f_or_list_exp = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_concat_exp(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["concat_exp"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(f_and((f_and(("|" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_or_list_exp(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["or_list_exp"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var a = []; a.push(f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["concat_exp"] ); })())); (function(a_) { for (var i_ = 0; i_ < a_.length ; i_++) { a.push(a_[i_]) }})(((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["or_list_exp"] ); })()))));  return a })())) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_or(((function () { return((v_MATCH.v_capture = [f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["concat_exp"] ); })())])) })()), function () { return 1})))) })())})) })())}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_or(((function () { return((v_MATCH.v_capture = [])) })()), function () { return 1})))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar$Regex.f_or_list_exp;  // v8 bug workaround
  // method rule
  Perlito$Grammar$Regex.f_rule = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and(((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(((function () { var v_last_pos = null;
(v_last_pos = v_MATCH.f_to());if ( f_bool(( f_bool(((function () { return(((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())) })())) ? false : true)) ) { (function () { (v_MATCH.v_to = v_last_pos); })() };return(1) })()), function () { return (f_and(("|" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((1)) })())})) })()), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_or_list_exp(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["or_list_exp"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = (function () { var tmp = {v_or_list: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["or_list_exp"] ); })())}; tmp.__proto__ = Rul$Or; return tmp })())) })()), function () { return 1}))}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Grammar$Regex.f_rule;  // v8 bug workaround
})();
;})();

// class GLOBAL
if (typeof GLOBAL != 'object') {
  GLOBAL = function() {};
  GLOBAL = new GLOBAL;
  GLOBAL.f_isa = function (s) { return s == 'GLOBAL' };
  GLOBAL.f_perl = function () { return 'GLOBAL.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = GLOBAL;
// use v6
;// class Rul
if (typeof Rul != 'object') {
  Rul = function() {};
  Rul = new Rul;
  Rul.f_isa = function (s) { return s == 'Rul' };
  Rul.f_perl = function () { return 'Rul.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Rul;
  // sub constant
  Rul.f_constant = function (v_str) {
    try { var v_len = null;
(v_len = f_chars(v_str));if ( f_bool((v_str == "\\")) ) { (function () { (v_str = "\\\\"); })() };if ( f_bool((v_str == "'")) ) { (function () { (v_str = "\\'"); })() };if ( f_bool((v_len)) ) { return (function () { return((f_string("( '") + f_string(v_str) + f_string("' eq substr( $str, $MATCH.to, ") + f_string(v_len) + f_string(") ") + f_string("&& ( $MATCH.to = ") + f_string(v_len) + f_string(" + $MATCH.to )") + f_string(")"))) })() } else { return (function () { throw("1") })() } } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
})();
;// class Rul::Quantifier
if (typeof Rul$Quantifier != 'object') {
  Rul$Quantifier = function() {};
  Rul$Quantifier = new Rul$Quantifier;
  Rul$Quantifier.f_isa = function (s) { return s == 'Rul::Quantifier' };
  Rul$Quantifier.f_perl = function () { return 'Rul::Quantifier.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Rul$Quantifier;
  // accessor term
  Rul$Quantifier.v_term = null;
  Rul$Quantifier.f_term = function () { return this.v_term }
  // accessor quant
  Rul$Quantifier.v_quant = null;
  Rul$Quantifier.f_quant = function () { return this.v_quant }
  // accessor greedy
  Rul$Quantifier.v_greedy = null;
  Rul$Quantifier.f_greedy = function () { return this.v_greedy }
  // accessor ws1
  Rul$Quantifier.v_ws1 = null;
  Rul$Quantifier.f_ws1 = function () { return this.v_ws1 }
  // accessor ws2
  Rul$Quantifier.v_ws2 = null;
  Rul$Quantifier.f_ws2 = function () { return this.v_ws2 }
  // accessor ws3
  Rul$Quantifier.v_ws3 = null;
  Rul$Quantifier.f_ws3 = function () { return this.v_ws3 }
  // method emit_perl6
  Rul$Quantifier.f_emit_perl6 = function () {
    var v_self = this;
    try { if ( f_bool(f_and(((v_self.v_quant == "")), function () { return ((v_self.v_greedy == ""))})) ) { (function () { throw(v_self.v_term.f_emit_perl6()); })() };if ( f_bool(f_and(((v_self.v_quant == "+")), function () { return ((v_self.v_greedy == ""))})) ) { (function () { v_self.v_term.f_set_captures_to_array();throw((f_string("(do { ") + f_string("my $last_match_null = 0; ") + f_string("my $last_pos = $MATCH.to; ") + f_string("my $count = 0; ") + f_string("while ") + f_string(v_self.v_term.f_emit_perl6()) + f_string(" && ($last_match_null < 2) ") + f_string("{ ") + f_string("if $last_pos == $MATCH.to() { ") + f_string("$last_match_null = $last_match_null + 1; ") + f_string("} ") + f_string("else { ") + f_string("$last_match_null = 0; ") + f_string("}; ") + f_string("$last_pos = $MATCH.to; ") + f_string("$count = $count + 1; ") + f_string("}; ") + f_string("$MATCH.to = $last_pos; ") + f_string("$count > 0; ") + f_string("})"))); })() };if ( f_bool(f_and(((v_self.v_quant == "*")), function () { return ((v_self.v_greedy == ""))})) ) { (function () { v_self.v_term.f_set_captures_to_array();throw((f_string("(do { ") + f_string("my $last_match_null = 0; ") + f_string("my $last_pos = $MATCH.to; ") + f_string("while ") + f_string(v_self.v_term.f_emit_perl6()) + f_string(" && ($last_match_null < 2) ") + f_string("{ ") + f_string("if $last_pos == $MATCH.to() { ") + f_string("$last_match_null = $last_match_null + 1; ") + f_string("} ") + f_string("else { ") + f_string("$last_match_null = 0; ") + f_string("}; ") + f_string("$last_pos = $MATCH.to; ") + f_string("}; ") + f_string("$MATCH.to = $last_pos; ") + f_string("1 ") + f_string("})"))); })() };if ( f_bool(f_and(((v_self.v_quant == "?")), function () { return ((v_self.v_greedy == ""))})) ) { (function () { v_self.v_term.f_set_captures_to_array();throw((f_string("(do { ") + f_string("my $last_pos = $MATCH.to; ") + f_string("if !(do {") + f_string(v_self.v_term.f_emit_perl6()) + f_string("}) ") + f_string("{ ") + f_string("$MATCH.to = $last_pos; ") + f_string("}; ") + f_string("1 ") + f_string("})"))); })() };f_warn((f_string("Rul::Quantifier: ") + f_string(f_perl(v_self, ((f_string(" not implemented"))).f_string()))));return(v_self.v_term.f_emit_perl6()) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$Quantifier.f_emit_perl6;  // v8 bug workaround
  // method set_captures_to_array
  Rul$Quantifier.f_set_captures_to_array = function () {
    var v_self = this;
    try { return(v_self.v_term.f_set_captures_to_array()) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$Quantifier.f_set_captures_to_array;  // v8 bug workaround
})();
;// class Rul::Or
if (typeof Rul$Or != 'object') {
  Rul$Or = function() {};
  Rul$Or = new Rul$Or;
  Rul$Or.f_isa = function (s) { return s == 'Rul::Or' };
  Rul$Or.f_perl = function () { return 'Rul::Or.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Rul$Or;
  // accessor or_list
  Rul$Or.v_or_list = null;
  Rul$Or.f_or_list = function () { return this.v_or_list }
  // method emit_perl6
  Rul$Or.f_emit_perl6 = function () {
    var v_self = this;
    try { return((f_string("(do { ") + f_string("my $pos1 = $MATCH.to; (do { ") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_perl6() ) }; return out; })(v_self.v_or_list)).join("}) || (do { $MATCH.to = $pos1; ")) + f_string("}) })"))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$Or.f_emit_perl6;  // v8 bug workaround
  // method set_captures_to_array
  Rul$Or.f_set_captures_to_array = function () {
    var v_self = this;
    try { return((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_set_captures_to_array() ) }; return out; })(v_self.v_or_list)) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$Or.f_set_captures_to_array;  // v8 bug workaround
})();
;// class Rul::Concat
if (typeof Rul$Concat != 'object') {
  Rul$Concat = function() {};
  Rul$Concat = new Rul$Concat;
  Rul$Concat.f_isa = function (s) { return s == 'Rul::Concat' };
  Rul$Concat.f_perl = function () { return 'Rul::Concat.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Rul$Concat;
  // accessor concat
  Rul$Concat.v_concat = null;
  Rul$Concat.f_concat = function () { return this.v_concat }
  // method emit_perl6
  Rul$Concat.f_emit_perl6 = function () {
    var v_self = this;
    try { return((f_string("(") + f_string(((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_emit_perl6() ) }; return out; })(v_self.v_concat)).join(" && ")) + f_string(")"))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$Concat.f_emit_perl6;  // v8 bug workaround
  // method set_captures_to_array
  Rul$Concat.f_set_captures_to_array = function () {
    var v_self = this;
    try { return((function (a_) { var out = []; if ( a_ == null ) { return out }; for(var i = 0; i < a_.length; i++) { out.push( a_[i].f_set_captures_to_array() ) }; return out; })(v_self.v_concat)) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$Concat.f_set_captures_to_array;  // v8 bug workaround
})();
;// class Rul::Subrule
if (typeof Rul$Subrule != 'object') {
  Rul$Subrule = function() {};
  Rul$Subrule = new Rul$Subrule;
  Rul$Subrule.f_isa = function (s) { return s == 'Rul::Subrule' };
  Rul$Subrule.f_perl = function () { return 'Rul::Subrule.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Rul$Subrule;
  // accessor metasyntax
  Rul$Subrule.v_metasyntax = null;
  Rul$Subrule.f_metasyntax = function () { return this.v_metasyntax }
  // accessor captures
  Rul$Subrule.v_captures = null;
  Rul$Subrule.f_captures = function () { return this.v_captures }
  // method emit_perl6
  Rul$Subrule.f_emit_perl6 = function () {
    var v_self = this;
    try { var v_meth = null;
var v_code = null;
(v_meth = ( f_bool((f_add(1, f_index(v_self.v_metasyntax, ".")))) ? v_self.v_metasyntax : ((f_string("$grammar.") + f_string(v_self.v_metasyntax)))));if ( f_bool((v_self.v_captures == 1)) ) { (function () { (v_code = (f_string("if $m2 { $MATCH.to = $m2.to; $MATCH{'") + f_string(v_self.v_metasyntax) + f_string("'} = $m2; 1 } else { False }; "))); })() } else { (function () { if ( f_bool((v_self.v_captures > 1)) ) { (function () { (v_code = (f_string("if $m2 { ") + f_string("$MATCH.to = $m2.to; ") + f_string("if exists $MATCH{'") + f_string(v_self.v_metasyntax) + f_string("'} { ") + f_string("($MATCH{'") + f_string(v_self.v_metasyntax) + f_string("'}).push( $m2 ); ") + f_string("} ") + f_string("else { ") + f_string("$MATCH{'") + f_string(v_self.v_metasyntax) + f_string("'} = [ $m2 ]; ") + f_string("}; ") + f_string("1 ") + f_string("} else { False }; "))); })() } else { (function () { (v_code = "if $m2 { $MATCH.to = $m2.to; 1 } else { False }; "); })() }; })() };return((f_string("(do { ") + f_string("my $m2 = ") + f_string(v_meth) + f_string("($str, $MATCH.to); ") + f_string(v_code) + f_string("})"))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$Subrule.f_emit_perl6;  // v8 bug workaround
  // method set_captures_to_array
  Rul$Subrule.f_set_captures_to_array = function () {
    var v_self = this;
    try { if ( f_bool((v_self.v_captures > 0)) ) { return (function () { return((v_self.v_captures = f_add(v_self.v_captures, 1))) })() } } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$Subrule.f_set_captures_to_array;  // v8 bug workaround
})();
;// class Rul::Var
if (typeof Rul$Var != 'object') {
  Rul$Var = function() {};
  Rul$Var = new Rul$Var;
  Rul$Var.f_isa = function (s) { return s == 'Rul::Var' };
  Rul$Var.f_perl = function () { return 'Rul::Var.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Rul$Var;
  // accessor sigil
  Rul$Var.v_sigil = null;
  Rul$Var.f_sigil = function () { return this.v_sigil }
  // accessor twigil
  Rul$Var.v_twigil = null;
  Rul$Var.f_twigil = function () { return this.v_twigil }
  // accessor name
  Rul$Var.v_name = null;
  Rul$Var.f_name = function () { return this.v_name }
  // method emit_perl6
  Rul$Var.f_emit_perl6 = function () {
    var v_self = this;
    try { var v_table = null;
(v_table = (function () { var a = {}; a["$"] = "$"; a["@"] = "$List_"; a["%"] = "$Hash_"; a["&"] = "$Code_"; return a })());return((f_string((function () { if (v_table == null) { v_table = {} }; return (v_table[v_self.v_sigil] ); })()) + f_string(v_self.v_name))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$Var.f_emit_perl6;  // v8 bug workaround
})();
;// class Rul::Constant
if (typeof Rul$Constant != 'object') {
  Rul$Constant = function() {};
  Rul$Constant = new Rul$Constant;
  Rul$Constant.f_isa = function (s) { return s == 'Rul::Constant' };
  Rul$Constant.f_perl = function () { return 'Rul::Constant.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Rul$Constant;
  // accessor constant
  Rul$Constant.v_constant = null;
  Rul$Constant.f_constant = function () { return this.v_constant }
  // method emit_perl6
  Rul$Constant.f_emit_perl6 = function () {
    var v_self = this;
    try { var v_str = null;
(v_str = v_self.v_constant);return(Rul.f_constant(v_str)) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$Constant.f_emit_perl6;  // v8 bug workaround
  // method set_captures_to_array
  Rul$Constant.f_set_captures_to_array = function () {
    var v_self = this;
    try {  } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$Constant.f_set_captures_to_array;  // v8 bug workaround
})();
;// class Rul::Dot
if (typeof Rul$Dot != 'object') {
  Rul$Dot = function() {};
  Rul$Dot = new Rul$Dot;
  Rul$Dot.f_isa = function (s) { return s == 'Rul::Dot' };
  Rul$Dot.f_perl = function () { return 'Rul::Dot.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Rul$Dot;
  // method emit_perl6
  Rul$Dot.f_emit_perl6 = function () {
    var v_self = this;
    try { return((f_string("( '' ne substr( $str, $MATCH.to, 1 ) ") + f_string("&& ($MATCH.to = 1 + $MATCH.to)") + f_string(")"))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$Dot.f_emit_perl6;  // v8 bug workaround
  // method set_captures_to_array
  Rul$Dot.f_set_captures_to_array = function () {
    var v_self = this;
    try {  } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$Dot.f_set_captures_to_array;  // v8 bug workaround
})();
;// class Rul::SpecialChar
if (typeof Rul$SpecialChar != 'object') {
  Rul$SpecialChar = function() {};
  Rul$SpecialChar = new Rul$SpecialChar;
  Rul$SpecialChar.f_isa = function (s) { return s == 'Rul::SpecialChar' };
  Rul$SpecialChar.f_perl = function () { return 'Rul::SpecialChar.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Rul$SpecialChar;
  // accessor char
  Rul$SpecialChar.v_char = null;
  Rul$SpecialChar.f_char = function () { return this.v_char }
  // method emit_perl6
  Rul$SpecialChar.f_emit_perl6 = function () {
    var v_self = this;
    try { var v_char = null;
(v_char = v_self.v_char);if ( f_bool((v_char == "n")) ) { (function () { throw((function () { var tmp = {v_metasyntax: "is_newline",v_captures: 0}; tmp.__proto__ = Rul$Subrule; return tmp })().f_emit_perl6()); })() };if ( f_bool((v_char == "N")) ) { (function () { throw((function () { var tmp = {v_metasyntax: "not_newline",v_captures: 0}; tmp.__proto__ = Rul$Subrule; return tmp })().f_emit_perl6()); })() };if ( f_bool((v_char == "d")) ) { (function () { throw((function () { var tmp = {v_metasyntax: "digit",v_captures: 0}; tmp.__proto__ = Rul$Subrule; return tmp })().f_emit_perl6()); })() };if ( f_bool((v_char == "s")) ) { (function () { throw((function () { var tmp = {v_metasyntax: "space",v_captures: 0}; tmp.__proto__ = Rul$Subrule; return tmp })().f_emit_perl6()); })() };if ( f_bool((v_char == "t")) ) { (function () { throw(Rul.f_constant(f_chr(9))); })() };throw(Rul.f_constant(v_char)) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$SpecialChar.f_emit_perl6;  // v8 bug workaround
  // method set_captures_to_array
  Rul$SpecialChar.f_set_captures_to_array = function () {
    var v_self = this;
    try {  } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$SpecialChar.f_set_captures_to_array;  // v8 bug workaround
})();
;// class Rul::Block
if (typeof Rul$Block != 'object') {
  Rul$Block = function() {};
  Rul$Block = new Rul$Block;
  Rul$Block.f_isa = function (s) { return s == 'Rul::Block' };
  Rul$Block.f_perl = function () { return 'Rul::Block.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Rul$Block;
  // accessor closure
  Rul$Block.v_closure = null;
  Rul$Block.f_closure = function () { return this.v_closure }
  // method emit_perl6
  Rul$Block.f_emit_perl6 = function () {
    var v_self = this;
    try { return((f_string("((do { ") + f_string(v_self.v_closure) + f_string(" }) || 1)"))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$Block.f_emit_perl6;  // v8 bug workaround
  // method set_captures_to_array
  Rul$Block.f_set_captures_to_array = function () {
    var v_self = this;
    try {  } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$Block.f_set_captures_to_array;  // v8 bug workaround
})();
;// class Rul::InterpolateVar
if (typeof Rul$InterpolateVar != 'object') {
  Rul$InterpolateVar = function() {};
  Rul$InterpolateVar = new Rul$InterpolateVar;
  Rul$InterpolateVar.f_isa = function (s) { return s == 'Rul::InterpolateVar' };
  Rul$InterpolateVar.f_perl = function () { return 'Rul::InterpolateVar.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Rul$InterpolateVar;
  // accessor var
  Rul$InterpolateVar.v_var = null;
  Rul$InterpolateVar.f_var = function () { return this.v_var }
  // method emit_perl6
  Rul$InterpolateVar.f_emit_perl6 = function () {
    var v_self = this;
    try { f_say((f_string("# TODO: interpolate var ") + f_string(v_self.v_var.f_emit_perl6()) + f_string("")));return(f_die()) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$InterpolateVar.f_emit_perl6;  // v8 bug workaround
  // method set_captures_to_array
  Rul$InterpolateVar.f_set_captures_to_array = function () {
    var v_self = this;
    try {  } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$InterpolateVar.f_set_captures_to_array;  // v8 bug workaround
})();
;// class Rul::NamedCapture
if (typeof Rul$NamedCapture != 'object') {
  Rul$NamedCapture = function() {};
  Rul$NamedCapture = new Rul$NamedCapture;
  Rul$NamedCapture.f_isa = function (s) { return s == 'Rul::NamedCapture' };
  Rul$NamedCapture.f_perl = function () { return 'Rul::NamedCapture.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Rul$NamedCapture;
  // accessor rule_exp
  Rul$NamedCapture.v_rule_exp = null;
  Rul$NamedCapture.f_rule_exp = function () { return this.v_rule_exp }
  // accessor capture_ident
  Rul$NamedCapture.v_capture_ident = null;
  Rul$NamedCapture.f_capture_ident = function () { return this.v_capture_ident }
  // method emit_perl6
  Rul$NamedCapture.f_emit_perl6 = function () {
    var v_self = this;
    try { f_say((f_string("# TODO: named capture ") + f_string(v_self.v_capture_ident) + f_string(" = ") + f_string(v_self.v_rule_exp.f_emit_perl6()) + f_string("")));return(f_die()) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$NamedCapture.f_emit_perl6;  // v8 bug workaround
  // method set_captures_to_array
  Rul$NamedCapture.f_set_captures_to_array = function () {
    var v_self = this;
    try { return(f_say("# TODO: named capture ")) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$NamedCapture.f_set_captures_to_array;  // v8 bug workaround
})();
;// class Rul::Before
if (typeof Rul$Before != 'object') {
  Rul$Before = function() {};
  Rul$Before = new Rul$Before;
  Rul$Before.f_isa = function (s) { return s == 'Rul::Before' };
  Rul$Before.f_perl = function () { return 'Rul::Before.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Rul$Before;
  // accessor rule_exp
  Rul$Before.v_rule_exp = null;
  Rul$Before.f_rule_exp = function () { return this.v_rule_exp }
  // method emit_perl6
  Rul$Before.f_emit_perl6 = function () {
    var v_self = this;
    try { return((f_string("(do { ") + f_string("my $tmp = $MATCH; ") + f_string("$MATCH = Perlito::Match.new( 'str' => $str, 'from' => $tmp.to, 'to' => $tmp.to, 'bool' => 1  ); ") + f_string("$MATCH.bool = ") + f_string(v_self.v_rule_exp.f_emit_perl6()) + f_string("; ") + f_string("$tmp.bool = ?$MATCH; ") + f_string("$MATCH = $tmp; ") + f_string("?$MATCH; ") + f_string("})"))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$Before.f_emit_perl6;  // v8 bug workaround
  // method set_captures_to_array
  Rul$Before.f_set_captures_to_array = function () {
    var v_self = this;
    try {  } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$Before.f_set_captures_to_array;  // v8 bug workaround
})();
;// class Rul::NotBefore
if (typeof Rul$NotBefore != 'object') {
  Rul$NotBefore = function() {};
  Rul$NotBefore = new Rul$NotBefore;
  Rul$NotBefore.f_isa = function (s) { return s == 'Rul::NotBefore' };
  Rul$NotBefore.f_perl = function () { return 'Rul::NotBefore.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Rul$NotBefore;
  // accessor rule_exp
  Rul$NotBefore.v_rule_exp = null;
  Rul$NotBefore.f_rule_exp = function () { return this.v_rule_exp }
  // method emit_perl6
  Rul$NotBefore.f_emit_perl6 = function () {
    var v_self = this;
    try { return((f_string("(do { ") + f_string("my $tmp = $MATCH; ") + f_string("$MATCH = Perlito::Match.new( 'str' => $str, 'from' => $tmp.to, 'to' => $tmp.to, 'bool' => 1  ); ") + f_string("$MATCH.bool = ") + f_string(v_self.v_rule_exp.f_emit_perl6()) + f_string("; ") + f_string("$tmp.bool = !$MATCH; ") + f_string("$MATCH = $tmp; ") + f_string("?$MATCH; ") + f_string("})"))) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$NotBefore.f_emit_perl6;  // v8 bug workaround
  // method set_captures_to_array
  Rul$NotBefore.f_set_captures_to_array = function () {
    var v_self = this;
    try {  } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$NotBefore.f_set_captures_to_array;  // v8 bug workaround
})();
;// class Rul::NegateCharClass
if (typeof Rul$NegateCharClass != 'object') {
  Rul$NegateCharClass = function() {};
  Rul$NegateCharClass = new Rul$NegateCharClass;
  Rul$NegateCharClass.f_isa = function (s) { return s == 'Rul::NegateCharClass' };
  Rul$NegateCharClass.f_perl = function () { return 'Rul::NegateCharClass.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Rul$NegateCharClass;
  // accessor chars
  Rul$NegateCharClass.v_chars = null;
  Rul$NegateCharClass.f_chars = function () { return this.v_chars }
  // method emit_perl6
  Rul$NegateCharClass.f_emit_perl6 = function () {
    var v_self = this;
    try { f_say((f_string("TODO NegateCharClass")));return(f_die()) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$NegateCharClass.f_emit_perl6;  // v8 bug workaround
})();
;// class Rul::CharClass
if (typeof Rul$CharClass != 'object') {
  Rul$CharClass = function() {};
  Rul$CharClass = new Rul$CharClass;
  Rul$CharClass.f_isa = function (s) { return s == 'Rul::CharClass' };
  Rul$CharClass.f_perl = function () { return 'Rul::CharClass.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Rul$CharClass;
  // accessor chars
  Rul$CharClass.v_chars = null;
  Rul$CharClass.f_chars = function () { return this.v_chars }
  // method emit_perl6
  Rul$CharClass.f_emit_perl6 = function () {
    var v_self = this;
    try { f_say((f_string("TODO CharClass")));return(f_die()) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$CharClass.f_emit_perl6;  // v8 bug workaround
})();
;// class Rul::Capture
if (typeof Rul$Capture != 'object') {
  Rul$Capture = function() {};
  Rul$Capture = new Rul$Capture;
  Rul$Capture.f_isa = function (s) { return s == 'Rul::Capture' };
  Rul$Capture.f_perl = function () { return 'Rul::Capture.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Rul$Capture;
  // accessor rule_exp
  Rul$Capture.v_rule_exp = null;
  Rul$Capture.f_rule_exp = function () { return this.v_rule_exp }
  // method emit_perl6
  Rul$Capture.f_emit_perl6 = function () {
    var v_self = this;
    try { f_say((f_string("TODO RulCapture")));return(f_die()) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$Capture.f_emit_perl6;  // v8 bug workaround
})();
;// class Rul::CaptureResult
if (typeof Rul$CaptureResult != 'object') {
  Rul$CaptureResult = function() {};
  Rul$CaptureResult = new Rul$CaptureResult;
  Rul$CaptureResult.f_isa = function (s) { return s == 'Rul::CaptureResult' };
  Rul$CaptureResult.f_perl = function () { return 'Rul::CaptureResult.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Rul$CaptureResult;
  // accessor rule_exp
  Rul$CaptureResult.v_rule_exp = null;
  Rul$CaptureResult.f_rule_exp = function () { return this.v_rule_exp }
  // method emit_perl6
  Rul$CaptureResult.f_emit_perl6 = function () {
    var v_self = this;
    try { f_say((f_string("TODO Rul::CaptureResult")));return(f_die()) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$CaptureResult.f_emit_perl6;  // v8 bug workaround
})();
;// class Rul::After
if (typeof Rul$After != 'object') {
  Rul$After = function() {};
  Rul$After = new Rul$After;
  Rul$After.f_isa = function (s) { return s == 'Rul::After' };
  Rul$After.f_perl = function () { return 'Rul::After.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Rul$After;
  // accessor rule_exp
  Rul$After.v_rule_exp = null;
  Rul$After.f_rule_exp = function () { return this.v_rule_exp }
  // method emit_perl6
  Rul$After.f_emit_perl6 = function () {
    var v_self = this;
    try { f_say((f_string("TODO Rul::After")));return(f_die()) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Rul$After.f_emit_perl6;  // v8 bug workaround
})();
;})();

// class GLOBAL
if (typeof GLOBAL != 'object') {
  GLOBAL = function() {};
  GLOBAL = new GLOBAL;
  GLOBAL.f_isa = function (s) { return s == 'GLOBAL' };
  GLOBAL.f_perl = function () { return 'GLOBAL.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = GLOBAL;
// class Perlito::Precedence
if (typeof Perlito$Precedence != 'object') {
  Perlito$Precedence = function() {};
  Perlito$Precedence = new Perlito$Precedence;
  Perlito$Precedence.f_isa = function (s) { return s == 'Perlito::Precedence' };
  Perlito$Precedence.f_perl = function () { return 'Perlito::Precedence.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Perlito$Precedence;
var v_Operator = null;
var v_Precedence = null;
var v_Assoc = null;
var v_Allow_space_before = null;
var v_Op1 = null;
var v_Op2 = null;
var v_End_token = null;
var v_prec = null;
  // accessor get_token
  Perlito$Precedence.v_get_token = null;
  Perlito$Precedence.f_get_token = function () { return this.v_get_token }
  // accessor reduce
  Perlito$Precedence.v_reduce = null;
  Perlito$Precedence.f_reduce = function () { return this.v_reduce }
  // accessor end_token
  Perlito$Precedence.v_end_token = null;
  Perlito$Precedence.f_end_token = function () { return this.v_end_token }
  // sub is_assoc_type
  Perlito$Precedence.f_is_assoc_type = function (v_assoc_type, v_op_name) {
    try { throw((function () { if (v_Assoc == null) { v_Assoc = {} }; if (v_Assoc[v_assoc_type] == null) { v_Assoc[v_assoc_type] = {} }; return (v_Assoc[v_assoc_type][v_op_name] ); })()) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  // sub is_fixity_type
  Perlito$Precedence.f_is_fixity_type = function (v_fixity_type, v_op_name) {
    try { throw((function () { if (v_Operator == null) { v_Operator = {} }; if (v_Operator[v_fixity_type] == null) { v_Operator[v_fixity_type] = {} }; return (v_Operator[v_fixity_type][v_op_name] ); })()) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  // sub is_term
  Perlito$Precedence.f_is_term = function (v_token) {
    try { return(f_or(((v_token[0] == "term")), function () { return ((v_token[0] == "postfix_or_term"))})) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  // sub is_ident_middle
  Perlito$Precedence.f_is_ident_middle = function (v_c) {
    try { return(f_or(f_or((f_and(((v_c >= "a")), function () { return ((v_c <= "z"))})), function () { return (f_and(((v_c >= "0")), function () { return ((v_c <= "9"))}))}), function () { return ((v_c == "_"))})) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  // method op_parse
  Perlito$Precedence.f_op_parse = function (v_str, v_pos) {
    var v_self = this;
    try { var v_from = null;
var v_c01 = null;
var v_c02 = null;
var v_hyper_left = null;
var v_hyper_right = null;
var v_op2 = null;
var v_op1 = null;
(v_from = v_pos);(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { (function (v_tok) { var v_l = null;
var v_s = null;
(v_l = f_chars(v_tok));(v_s = (v_str || "").substr(v_pos, v_l));if ( f_bool((v_s == v_tok)) ) { (function () { var v_c1 = null;
var v_c2 = null;
(v_c1 = (v_str || "").substr((f_add(v_pos, v_l) - 1), 1));(v_c2 = (v_str || "").substr(f_add(v_pos, v_l), 1));if ( f_bool(f_and(v__NAMESPACE.f_is_ident_middle(v_c1), function () { return (f_or(v__NAMESPACE.f_is_ident_middle(v_c2), function () { return (v_c2 == "(")}))})) ) { (function () {  })() } else { (function () { throw((function () { var tmp = {v_str: v_str,v_from: v_from,v_to: f_add(v_pos, 2),v_bool: 1,v_capture: ["end", v_s]}; tmp.__proto__ = Perlito$Match; return tmp })()); })() }; })() }; })(a_[i_]) } })((function () { var a = []; (function(a_) { for (var i_ = 0; i_ < a_.length ; i_++) { a.push(a_[i_]) }})(((v_End_token)));  return a })());(v_c01 = (v_str || "").substr(v_pos, 1));(v_c02 = (v_str || "").substr(v_pos, 2));(v_hyper_left = 0);(v_hyper_right = 0);if ( f_bool(f_or(((v_c01 == "«")), function () { return ((v_c01 == "»"))})) ) { (function () { (v_hyper_left = v_c01);(v_pos = f_add(v_pos, 1));(v_c02 = (v_str || "").substr(v_pos, 2)); })() } else { (function () { if ( f_bool(f_or(((v_c02 == "<<")), function () { return ((v_c02 == ">>"))})) ) { (function () { (v_hyper_left = v_c02);(v_pos = f_add(v_pos, 2));(v_c02 = (v_str || "").substr(v_pos, 2)); })() }; })() };(v_op2 = v_c02);if ( f_bool((v_Op2).hasOwnProperty(v_op2)) ) { (function () { var v_c1 = null;
var v_c2 = null;
(v_c1 = (v_str || "").substr(f_add(v_pos, 1), 1));(v_c2 = (v_str || "").substr(f_add(v_pos, 2), 1));if ( f_bool(f_and(v__NAMESPACE.f_is_ident_middle(v_c1), function () { return (f_or(v__NAMESPACE.f_is_ident_middle(v_c2), function () { return (v_c2 == "(")}))})) ) { (function () {  })() } else { (function () { var v_c01 = null;
var v_c02 = null;
(v_pos = f_add(v_pos, 2));(v_c01 = (v_str || "").substr(v_pos, 1));(v_c02 = (v_str || "").substr(v_pos, 2));if ( f_bool(f_or(((v_c01 == "«")), function () { return ((v_c01 == "»"))})) ) { (function () { (v_hyper_right = v_c01);(v_pos = f_add(v_pos, 1)); })() } else { (function () { if ( f_bool(f_or(((v_c02 == "<<")), function () { return ((v_c02 == ">>"))})) ) { (function () { (v_hyper_right = v_c02);(v_pos = f_add(v_pos, 2)); })() }; })() };throw((function () { var tmp = {v_str: v_str,v_from: v_from,v_to: v_pos,v_bool: 1,v_capture: ["op", v_op2, (function () { var a = {}; a["hyper_left"] = v_hyper_left; a["hyper_right"] = v_hyper_right; return a })()]}; tmp.__proto__ = Perlito$Match; return tmp })()); })() }; })() };(v_op1 = (v_str || "").substr(v_pos, 1));if ( f_bool((v_Op1).hasOwnProperty(v_op1)) ) { (function () { var v_c2 = null;
(v_c2 = (v_str || "").substr(f_add(v_pos, 1), 1));if ( f_bool(f_and(v__NAMESPACE.f_is_ident_middle(v_op1), function () { return (f_or(v__NAMESPACE.f_is_ident_middle(v_c2), function () { return (v_c2 == "(")}))})) ) { (function () {  })() } else { (function () { var v_c01 = null;
var v_c02 = null;
(v_pos = f_add(v_pos, 1));(v_c01 = (v_str || "").substr(v_pos, 1));(v_c02 = (v_str || "").substr(v_pos, 2));if ( f_bool(f_or(((v_c01 == "«")), function () { return ((v_c01 == "»"))})) ) { (function () { (v_hyper_right = v_c01);(v_pos = f_add(v_pos, 1)); })() } else { (function () { if ( f_bool(f_or(((v_c02 == "<<")), function () { return ((v_c02 == ">>"))})) ) { (function () { (v_hyper_right = v_c02);(v_pos = f_add(v_pos, 2)); })() }; })() };throw((function () { var tmp = {v_str: v_str,v_from: v_from,v_to: v_pos,v_bool: 1,v_capture: ["op", v_op1, (function () { var a = {}; a["hyper_left"] = v_hyper_left; a["hyper_right"] = v_hyper_right; return a })()]}; tmp.__proto__ = Perlito$Match; return tmp })()); })() }; })() };throw((function () { var tmp = {v_bool: 0}; tmp.__proto__ = Perlito$Match; return tmp })()) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Precedence.f_op_parse;  // v8 bug workaround
  // sub add_op
  Perlito$Precedence.f_add_op = function (v_fixity, v_name, v_precedence, v_param) {
    try { var v_assoc = null;
if ( f_bool(( f_bool(((v_param != null))) ? false : true)) ) { (function () { (v_param = (function () { var a = {}; ; return a })()); })() };(v_assoc = f_or((function () { if (v_param == null) { v_param = {} }; return (v_param["assoc"] ); })(), function () { return "left"}));(function () { if (v_Operator == null) { v_Operator = {} }; if (v_Operator[v_fixity] == null) { v_Operator[v_fixity] = {} }; return (v_Operator[v_fixity][v_name]  = 1); })();(function () { if (v_Precedence == null) { v_Precedence = {} }; return (v_Precedence[v_name]  = v_precedence); })();(function () { if (v_Assoc == null) { v_Assoc = {} }; if (v_Assoc[v_assoc] == null) { v_Assoc[v_assoc] = {} }; return (v_Assoc[v_assoc][v_name]  = 1); })();(function () { if (v_Allow_space_before == null) { v_Allow_space_before = {} }; if (v_Allow_space_before[v_fixity] == null) { v_Allow_space_before[v_fixity] = {} }; return (v_Allow_space_before[v_fixity][v_name]  = ( f_bool((function () { if (v_param == null) { v_param = {} }; return (v_param["no_space_before"] ); })()) ? false : true)); })();if ( f_bool(((f_chars(v_name)) == 1)) ) { return (function () { return((function () { if (v_Op1 == null) { v_Op1 = {} }; return (v_Op1[v_name]  = 1); })()) })() } else { return (function () { if ( f_bool(((f_chars(v_name)) == 2)) ) { return (function () { return((function () { if (v_Op2 == null) { v_Op2 = {} }; return (v_Op2[v_name]  = 1); })()) })() } })() } } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  // method precedence_parse
  Perlito$Precedence.f_precedence_parse = function () {
    var v_self = this;
    try { var v_get_token = null;
var v_reduce = null;
var v_last_end_token = null;
var v_op_stack = null;
var v_num_stack = null;
var v_last = null;
var v_last_has_space = null;
var v_token = null;
(v_get_token = v_self.f_get_token());(v_reduce = v_self.f_reduce());(v_last_end_token = v_End_token);(v_End_token = v_self.f_end_token());(v_op_stack = []);(v_num_stack = []);(v_last = ["op", "*start*"]);(v_last_has_space = false);(v_token = (v_get_token)());if ( f_bool(((v_token[0]) == "space")) ) { (function () { (v_token = (v_get_token)()); })() };for ( ; f_bool(f_and(((v_token != null)), function () { return ((v_token[0] != "end"))}));  ) { (function () { if ( f_bool(f_and(((v_token[1] == ",")), function () { return (f_or(((v_last[1] == "*start*")), function () { return ((v_last[1] == ","))}))})) ) { (function () { v_num_stack.push(["term", null]); })() };if ( f_bool(f_and((function () { if (v_Operator == null) { v_Operator = {} }; if (v_Operator["prefix"] == null) { v_Operator["prefix"] = {} }; return (v_Operator["prefix"][v_token[1]] ); })(), function () { return (f_or(((v_last[1] == "*start*")), function () { return ( f_bool((v__NAMESPACE.f_is_term(v_last))) ? false : true)}))})) ) { (function () { (v_token[0] = "prefix");v_op_stack.unshift(v_token); })() } else { (function () { if ( f_bool(f_and(f_and((function () { if (v_Operator == null) { v_Operator = {} }; if (v_Operator["postfix"] == null) { v_Operator["postfix"] = {} }; return (v_Operator["postfix"][v_token[1]] ); })(), function () { return v__NAMESPACE.f_is_term(v_last)}), function () { return (f_or((function () { if (v_Allow_space_before == null) { v_Allow_space_before = {} }; if (v_Allow_space_before["postfix"] == null) { v_Allow_space_before["postfix"] = {} }; return (v_Allow_space_before["postfix"][v_token[1]] ); })(), function () { return ( f_bool((v_last_has_space)) ? false : true)}))})) ) { (function () { var v_pr = null;
(v_pr = (function () { if (v_Precedence == null) { v_Precedence = {} }; return (v_Precedence[v_token[1]] ); })());for ( ; f_bool(f_and(f_elems(v_op_stack), function () { return ((v_pr <= (function () { if (v_Precedence == null) { v_Precedence = {} }; return (v_Precedence[(v_op_stack[0])[1]] ); })()))}));  ) { (function () { (v_reduce)(v_op_stack, v_num_stack); })() };if ( f_bool(((v_token[0]) != "postfix_or_term")) ) { (function () { (v_token[0] = "postfix"); })() };v_op_stack.unshift(v_token); })() } else { (function () { if ( f_bool(f_and(f_and(((v_token[1] == "block")), function () { return v__NAMESPACE.f_is_term(v_last)}), function () { return v_last_has_space})) ) { (function () { for ( ; f_bool(f_elems(v_op_stack));  ) { (function () { (v_reduce)(v_op_stack, v_num_stack); })() };v_num_stack.push(v_token);(v_End_token = v_last_end_token);throw(v_num_stack); })() } else { (function () { if ( f_bool(v__NAMESPACE.f_is_term(v_token)) ) { (function () { if ( f_bool(v__NAMESPACE.f_is_term(v_last)) ) { (function () { f_say((f_string("#      last:  ")), f_perl(v_last));f_say((f_string("#      token: ")), f_perl(v_token));f_say((f_string("#      space: ")), v_last_has_space);f_die((f_string("Value tokens must be separated by an operator"))); })() };(v_token[0] = "term");v_num_stack.push(v_token); })() } else { (function () { if ( f_bool((function () { if (v_Precedence == null) { v_Precedence = {} }; return (v_Precedence[v_token[1]] ); })()) ) { (function () { var v_pr = null;
(v_pr = (function () { if (v_Precedence == null) { v_Precedence = {} }; return (v_Precedence[v_token[1]] ); })());if ( f_bool((function () { if (v_Assoc == null) { v_Assoc = {} }; if (v_Assoc["right"] == null) { v_Assoc["right"] = {} }; return (v_Assoc["right"][v_token[1]] ); })()) ) { (function () { for ( ; f_bool(f_and(f_elems(v_op_stack), function () { return ((v_pr < (function () { if (v_Precedence == null) { v_Precedence = {} }; return (v_Precedence[(v_op_stack[0])[1]] ); })()))}));  ) { (function () { (v_reduce)(v_op_stack, v_num_stack); })() }; })() } else { (function () { for ( ; f_bool(f_and(f_elems(v_op_stack), function () { return ((v_pr <= (function () { if (v_Precedence == null) { v_Precedence = {} }; return (v_Precedence[(v_op_stack[0])[1]] ); })()))}));  ) { (function () { (v_reduce)(v_op_stack, v_num_stack); })() }; })() };if ( f_bool((function () { if (v_Operator == null) { v_Operator = {} }; if (v_Operator["ternary"] == null) { v_Operator["ternary"] = {} }; return (v_Operator["ternary"][v_token[1]] ); })()) ) { (function () { (v_token[0] = "ternary"); })() } else { (function () { (v_token[0] = "infix"); })() };v_op_stack.unshift(v_token); })() } else { (function () { f_die((f_string("Unknown token: '")), v_token[1], (f_string("'"))); })() }; })() }; })() }; })() }; })() };(v_last = v_token);(v_token = (v_get_token)());if ( f_bool((v_token[0] == "space")) ) { (function () { (v_token = (v_get_token)());(v_last_has_space = true); })() } else { (function () { (v_last_has_space = false); })() }; })() };if ( f_bool(f_and((v_token != null), function () { return ((v_token[0] != "end"))})) ) { (function () { f_die((f_string("Unexpected end token: ")), f_perl(v_token)); })() };for ( ; f_bool(f_elems(v_op_stack));  ) { (function () { (v_reduce)(v_op_stack, v_num_stack); })() };(v_End_token = v_last_end_token);throw(v_num_stack) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Precedence.f_precedence_parse;  // v8 bug workaround
(v_Operator = (function () { var a = {}; ; return a })());(v_Precedence = (function () { var a = {}; ; return a })());(v_Assoc = (function () { var a = {}; ; return a })());(v_Allow_space_before = (function () { var a = {}; ; return a })());(v_prec = 100);v__NAMESPACE.f_add_op("postfix", ".( )", v_prec, (function () { var a = {}; a["no_space_before"] = true; return a })());v__NAMESPACE.f_add_op("postfix", ".[ ]", v_prec, (function () { var a = {}; a["no_space_before"] = true; return a })());v__NAMESPACE.f_add_op("postfix", ".{ }", v_prec, (function () { var a = {}; a["no_space_before"] = true; return a })());v__NAMESPACE.f_add_op("postfix", "( )", v_prec, (function () { var a = {}; a["no_space_before"] = true; return a })());v__NAMESPACE.f_add_op("postfix", "[ ]", v_prec, (function () { var a = {}; a["no_space_before"] = true; return a })());v__NAMESPACE.f_add_op("postfix", "funcall", v_prec, (function () { var a = {}; a["no_space_before"] = true; return a })());v__NAMESPACE.f_add_op("postfix", "funcall_no_params", v_prec, (function () { var a = {}; a["no_space_before"] = true; return a })());v__NAMESPACE.f_add_op("postfix", "methcall", v_prec, (function () { var a = {}; a["no_space_before"] = true; return a })());v__NAMESPACE.f_add_op("postfix", "methcall_no_params", v_prec, (function () { var a = {}; a["no_space_before"] = true; return a })());v__NAMESPACE.f_add_op("postfix", "block", v_prec, (function () { var a = {}; a["no_space_before"] = true; return a })());v__NAMESPACE.f_add_op("postfix", "hash", v_prec, (function () { var a = {}; a["no_space_before"] = true; return a })());(v_prec = (v_prec - 1));v__NAMESPACE.f_add_op("prefix", "++", v_prec);v__NAMESPACE.f_add_op("prefix", "--", v_prec);v__NAMESPACE.f_add_op("postfix", "++", v_prec, (function () { var a = {}; a["no_space_before"] = true; return a })());v__NAMESPACE.f_add_op("postfix", "--", v_prec, (function () { var a = {}; a["no_space_before"] = true; return a })());(v_prec = (v_prec - 1));v__NAMESPACE.f_add_op("infix", "**", v_prec, (function () { var a = {}; a["assoc"] = "right"; return a })());(v_prec = (v_prec - 1));v__NAMESPACE.f_add_op("prefix", "+", v_prec);v__NAMESPACE.f_add_op("prefix", "-", v_prec);v__NAMESPACE.f_add_op("prefix", "$", v_prec);v__NAMESPACE.f_add_op("prefix", "@", v_prec);v__NAMESPACE.f_add_op("prefix", "%", v_prec);v__NAMESPACE.f_add_op("prefix", "!", v_prec);v__NAMESPACE.f_add_op("prefix", "?", v_prec);(v_prec = (v_prec - 1));v__NAMESPACE.f_add_op("infix", "*", v_prec);v__NAMESPACE.f_add_op("infix", "/", v_prec);(v_prec = (v_prec - 1));v__NAMESPACE.f_add_op("infix", "+", v_prec);v__NAMESPACE.f_add_op("infix", "-", v_prec);(v_prec = (v_prec - 1));v__NAMESPACE.f_add_op("infix", "~", v_prec, (function () { var a = {}; a["assoc"] = "list"; return a })());v__NAMESPACE.f_add_op("prefix", "~", v_prec);(v_prec = (v_prec - 1));v__NAMESPACE.f_add_op("infix", "&", v_prec, (function () { var a = {}; a["assoc"] = "list"; return a })());v__NAMESPACE.f_add_op("prefix", "&", v_prec);(v_prec = (v_prec - 1));v__NAMESPACE.f_add_op("infix", "|", v_prec, (function () { var a = {}; a["assoc"] = "list"; return a })());v__NAMESPACE.f_add_op("prefix", "|", v_prec);(v_prec = (v_prec - 1));v__NAMESPACE.f_add_op("infix", "<=>", v_prec);v__NAMESPACE.f_add_op("infix", "leg", v_prec);v__NAMESPACE.f_add_op("infix", "cmp", v_prec);v__NAMESPACE.f_add_op("infix", "does", v_prec);v__NAMESPACE.f_add_op("infix", "but", v_prec);v__NAMESPACE.f_add_op("infix", "..", v_prec);v__NAMESPACE.f_add_op("infix", "^..", v_prec);v__NAMESPACE.f_add_op("infix", "..^", v_prec);v__NAMESPACE.f_add_op("infix", "^..^", v_prec);(v_prec = (v_prec - 1));v__NAMESPACE.f_add_op("infix", "ne", v_prec, (function () { var a = {}; a["assoc"] = "chain"; return a })());v__NAMESPACE.f_add_op("infix", "eq", v_prec, (function () { var a = {}; a["assoc"] = "chain"; return a })());v__NAMESPACE.f_add_op("infix", "lt", v_prec, (function () { var a = {}; a["assoc"] = "chain"; return a })());v__NAMESPACE.f_add_op("infix", "le", v_prec, (function () { var a = {}; a["assoc"] = "chain"; return a })());v__NAMESPACE.f_add_op("infix", "gt", v_prec, (function () { var a = {}; a["assoc"] = "chain"; return a })());v__NAMESPACE.f_add_op("infix", "ge", v_prec, (function () { var a = {}; a["assoc"] = "chain"; return a })());v__NAMESPACE.f_add_op("infix", "<=", v_prec, (function () { var a = {}; a["assoc"] = "chain"; return a })());v__NAMESPACE.f_add_op("infix", ">=", v_prec, (function () { var a = {}; a["assoc"] = "chain"; return a })());v__NAMESPACE.f_add_op("infix", "==", v_prec, (function () { var a = {}; a["assoc"] = "chain"; return a })());v__NAMESPACE.f_add_op("infix", "!=", v_prec, (function () { var a = {}; a["assoc"] = "chain"; return a })());v__NAMESPACE.f_add_op("infix", "<", v_prec, (function () { var a = {}; a["assoc"] = "chain"; return a })());v__NAMESPACE.f_add_op("infix", ">", v_prec, (function () { var a = {}; a["assoc"] = "chain"; return a })());v__NAMESPACE.f_add_op("infix", "~~", v_prec, (function () { var a = {}; a["assoc"] = "chain"; return a })());(v_prec = (v_prec - 1));v__NAMESPACE.f_add_op("infix", "&&", v_prec);(v_prec = (v_prec - 1));v__NAMESPACE.f_add_op("infix", "||", v_prec);v__NAMESPACE.f_add_op("infix", "//", v_prec);(v_prec = (v_prec - 1));v__NAMESPACE.f_add_op("ternary", "?? !!", v_prec);(v_prec = (v_prec - 1));v__NAMESPACE.f_add_op("infix", "=", v_prec, (function () { var a = {}; a["assoc"] = "right"; return a })());v__NAMESPACE.f_add_op("infix", ":=", v_prec, (function () { var a = {}; a["assoc"] = "right"; return a })());(v_prec = (v_prec - 1));v__NAMESPACE.f_add_op("prefix", "not", v_prec);(v_prec = (v_prec - 1));v__NAMESPACE.f_add_op("infix", "=>", v_prec);(v_prec = (v_prec - 1));v__NAMESPACE.f_add_op("list", ",", v_prec, (function () { var a = {}; a["assoc"] = "list"; return a })());(v_prec = (v_prec - 1));v__NAMESPACE.f_add_op("infix", "and", v_prec);(v_prec = (v_prec - 1));v__NAMESPACE.f_add_op("infix", "or", v_prec);(v_prec = (v_prec - 1));v__NAMESPACE.f_add_op("infix", "*start*", v_prec);})();
;})();

// class GLOBAL
if (typeof GLOBAL != 'object') {
  GLOBAL = function() {};
  GLOBAL = new GLOBAL;
  GLOBAL.f_isa = function (s) { return s == 'GLOBAL' };
  GLOBAL.f_perl = function () { return 'GLOBAL.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = GLOBAL;
// class Perlito::Expression
if (typeof Perlito$Expression != 'object') {
  Perlito$Expression = function() {};
  Perlito$Expression = new Perlito$Expression;
  Perlito$Expression.f_isa = function (s) { return s == 'Perlito::Expression' };
  Perlito$Expression.f_perl = function () { return 'Perlito::Expression.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Perlito$Expression;
var v_reduce_to_ast = null;
  // sub expand_list
  Perlito$Expression.f_expand_list = function (v_param_list) {
    try { if ( f_bool(f_and((f_isa(v_param_list, "Apply")), function () { return (((v_param_list.f_code()) == "list:<,>"))})) ) { return (function () { var v_args = null;
(v_args = []);(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { (function (v_v) { if ( f_bool((v_v != null)) ) { (function () { v_args.push(v_v); })() }; })(a_[i_]) } })((function () { var a = []; (function(a_) { for (var i_ = 0; i_ < a_.length ; i_++) { a.push(a_[i_]) }})(((v_param_list.f_arguments())));  return a })());throw(v_args) })() } else { return (function () { if ( f_bool((v_param_list == "*undef*")) ) { return (function () { throw([]) })() } else { return (function () { throw([v_param_list]) })() } })() } } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  // sub block_or_hash
  Perlito$Expression.f_block_or_hash = function (v_o) {
    try { var v_stmts = null;
var v_stmt = null;
if ( f_bool((v_o.f_sig() != null)) ) { (function () { throw(v_o); })() };(v_stmts = v_o.f_stmts());if ( f_bool(f_or((( f_bool(((v_stmts != null))) ? false : true)), function () { return (((f_elems(v_stmts)) == 0))})) ) { (function () { throw((function () { var tmp = {v_hash1: []}; tmp.__proto__ = Lit$Hash; return tmp })()); })() };if ( f_bool(((f_elems(v_stmts)) != 1)) ) { (function () { throw(v_o); })() };(v_stmt = v_stmts[0]);if ( f_bool(( f_bool((f_isa(v_stmt, "Apply"))) ? false : true)) ) { (function () { throw(v_o); })() };if ( f_bool(((v_stmt.f_code()) == "infix:<=>>")) ) { (function () { throw((function () { var tmp = {v_hash1: [v_stmt]}; tmp.__proto__ = Lit$Hash; return tmp })()); })() };if ( f_bool(((v_stmt.f_code()) != "list:<,>")) ) { (function () { throw(v_o); })() };(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { (function (v_item) { if ( f_bool(f_and(f_isa(v_item, "Apply"), function () { return ((v_item.f_code()) == "infix:<=>>")})) ) { (function () { throw((function () { var tmp = {v_hash1: v__NAMESPACE.f_expand_list(v_stmt)}; tmp.__proto__ = Lit$Hash; return tmp })()); })() }; })(a_[i_]) } })((function () { var a = []; (function(a_) { for (var i_ = 0; i_ < a_.length ; i_++) { a.push(a_[i_]) }})(((v_stmt.f_arguments())));  return a })());throw(v_o) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  // sub pop_term
  Perlito$Expression.f_pop_term = function (v_num_stack) {
    try { var v_v = null;
(v_v = v_num_stack.pop());if ( f_bool(f_isa(v_v, "Array")) ) { (function () { if ( f_bool((v_v[1] == "methcall_no_params")) ) { (function () { (v_v = (function () { var tmp = {v_invocant: null,v_method: v_v[2],v_arguments: null,v_hyper: v_v[3]}; tmp.__proto__ = Call; return tmp })());throw(v_v); })() };if ( f_bool((v_v[1] == "funcall_no_params")) ) { (function () { (v_v = (function () { var tmp = {v_code: v_v[3],v_arguments: null,v_namespace: v_v[2]}; tmp.__proto__ = Apply; return tmp })());throw(v_v); })() };if ( f_bool((v_v[1] == "methcall")) ) { (function () { var v_param_list = null;
if ( f_bool((function () { if ((v_v[3]) == null) { (v_v[3]) = {} }; return ((v_v[3])["end_block"] ); })()) ) { (function () { v_num_stack.unshift((function () { if ((v_v[3]) == null) { (v_v[3]) = {} }; return ((v_v[3])["end_block"] ); })()); })() };(v_param_list = v__NAMESPACE.f_expand_list((function () { if ((v_v[3]) == null) { (v_v[3]) = {} }; return ((v_v[3])["exp"] ); })()));(v_v = (function () { var tmp = {v_invocant: null,v_method: v_v[2],v_arguments: v_param_list,v_hyper: v_v[4]}; tmp.__proto__ = Call; return tmp })());throw(v_v); })() };if ( f_bool((v_v[1] == "funcall")) ) { (function () { var v_param_list = null;
if ( f_bool((function () { if ((v_v[4]) == null) { (v_v[4]) = {} }; return ((v_v[4])["end_block"] ); })()) ) { (function () { v_num_stack.unshift((function () { if ((v_v[4]) == null) { (v_v[4]) = {} }; return ((v_v[4])["end_block"] ); })()); })() };(v_param_list = v__NAMESPACE.f_expand_list((function () { if ((v_v[4]) == null) { (v_v[4]) = {} }; return ((v_v[4])["exp"] ); })()));(v_v = (function () { var tmp = {v_code: v_v[3],v_arguments: v_param_list,v_namespace: v_v[2]}; tmp.__proto__ = Apply; return tmp })());throw(v_v); })() };if ( f_bool((v_v[1] == "( )")) ) { (function () { var v_param_list = null;
(v_param_list = v__NAMESPACE.f_expand_list(v_v[2]));(v_v = (function () { var tmp = {v_code: "circumfix:<( )>",v_arguments: v_param_list,v_namespace: ""}; tmp.__proto__ = Apply; return tmp })());throw(v_v); })() };if ( f_bool((v_v[1] == "[ ]")) ) { (function () { var v_param_list = null;
(v_param_list = v__NAMESPACE.f_expand_list(v_v[2]));(v_v = (function () { var tmp = {v_array1: v_param_list}; tmp.__proto__ = Lit$Array; return tmp })());throw(v_v); })() };if ( f_bool((v_v[1] == "block")) ) { (function () { (v_v = (function () { var tmp = {v_stmts: v_v[2],v_sig: v_v[3]}; tmp.__proto__ = Lit$Block; return tmp })());(v_v = v__NAMESPACE.f_block_or_hash(v_v));throw(v_v); })() };if ( f_bool((v_v[1] == ".( )")) ) { (function () { (v_v = (function () { var tmp = {v_invocant: null,v_method: "postcircumfix:<( )>",v_arguments: v_v[2],v_hyper: 0}; tmp.__proto__ = Call; return tmp })());throw(v_v); })() };if ( f_bool((v_v[1] == ".[ ]")) ) { (function () { (v_v = (function () { var tmp = {v_obj: null,v_index_exp: v_v[2]}; tmp.__proto__ = Index; return tmp })());throw(v_v); })() };if ( f_bool((v_v[1] == ".{ }")) ) { (function () { (v_v = (function () { var tmp = {v_obj: null,v_index_exp: v_v[2]}; tmp.__proto__ = Lookup; return tmp })());throw(v_v); })() };if ( f_bool(f_and(f_isa((v_v[1]), "Array"), function () { return (((f_elems((v_v[1]))) == 2))})) ) { (function () { (v_v = (function () { var tmp = {v_code: "pair",v_arguments: v_v[1],v_namespace: ""}; tmp.__proto__ = Apply; return tmp })());throw(v_v); })() };throw(v_v[1]); })() };throw(v_v) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  // sub reduce_postfix
  Perlito$Expression.f_reduce_postfix = function (v_op, v_value) {
    try { var v_v = null;
(v_v = v_op);if ( f_bool((v_v[1] == "methcall_no_params")) ) { (function () { (v_v = (function () { var tmp = {v_invocant: v_value,v_method: v_v[2],v_arguments: null,v_hyper: v_v[3]}; tmp.__proto__ = Call; return tmp })());throw(v_v); })() };if ( f_bool((v_v[1] == "funcall_no_params")) ) { (function () { f_die((f_string("unexpected function call")));f_push(v_v, v_value);throw(v_v); })() };if ( f_bool((v_v[1] == "methcall")) ) { (function () { var v_param_list = null;
(v_param_list = v__NAMESPACE.f_expand_list((function () { if ((v_v[3]) == null) { (v_v[3]) = {} }; return ((v_v[3])["exp"] ); })()));(v_v = (function () { var tmp = {v_invocant: v_value,v_method: v_v[2],v_arguments: v_param_list,v_hyper: v_v[4]}; tmp.__proto__ = Call; return tmp })());throw(v_v); })() };if ( f_bool((v_v[1] == "funcall")) ) { (function () { f_die((f_string("unexpected function call")));f_push(v_v, v_value);throw(v_v); })() };if ( f_bool((v_v[1] == "( )")) ) { (function () { var v_param_list = null;
(v_param_list = v__NAMESPACE.f_expand_list(v_v[2]));if ( f_bool(f_and(f_isa(v_value, "Apply"), function () { return ( f_bool(((v_value.f_arguments() != null))) ? false : true)})) ) { (function () { (v_value.v_arguments = v_param_list);throw(v_value); })() };if ( f_bool(f_and(f_isa(v_value, "Call"), function () { return ( f_bool(((v_value.f_arguments() != null))) ? false : true)})) ) { (function () { (v_value.v_arguments = v_param_list);throw(v_value); })() };(v_v = (function () { var tmp = {v_invocant: v_value,v_method: "postcircumfix:<( )>",v_arguments: v_param_list,v_hyper: 0}; tmp.__proto__ = Call; return tmp })());throw(v_v); })() };if ( f_bool((v_v[1] == "[ ]")) ) { (function () { (v_v = (function () { var tmp = {v_obj: v_value,v_index_exp: v_v[2]}; tmp.__proto__ = Index; return tmp })());throw(v_v); })() };if ( f_bool((v_v[1] == "block")) ) { (function () { (v_v = (function () { var tmp = {v_obj: v_value,v_index_exp: (v_v[2])[0]}; tmp.__proto__ = Lookup; return tmp })());throw(v_v); })() };if ( f_bool((v_v[1] == ".( )")) ) { (function () { var v_param_list = null;
(v_param_list = v__NAMESPACE.f_expand_list(v_v[2]));(v_v = (function () { var tmp = {v_invocant: v_value,v_method: "postcircumfix:<( )>",v_arguments: v_param_list,v_hyper: 0}; tmp.__proto__ = Call; return tmp })());throw(v_v); })() };if ( f_bool((v_v[1] == ".[ ]")) ) { (function () { (v_v = (function () { var tmp = {v_invocant: v_value,v_method: "postcircumfix:<[ ]>",v_arguments: v_v[2],v_hyper: 0}; tmp.__proto__ = Call; return tmp })());throw(v_v); })() };if ( f_bool((v_v[1] == ".{ }")) ) { (function () { (v_v = (function () { var tmp = {v_invocant: v_value,v_method: "postcircumfix:<{ }>",v_arguments: v_v[2],v_hyper: 0}; tmp.__proto__ = Call; return tmp })());throw(v_v); })() };f_push(v_op, v_value);throw(v_op) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  // method capture_name
  Perlito$Expression.f_capture_name = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_full_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.full_ident"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_last_pos = null;
(v_last_pos = v_MATCH.f_to());if ( f_bool(( f_bool(((function () { return(((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and((f_and(("." == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());if ( f_bool((v_MATCH).hasOwnProperty("Perlito::Grammar.ident")) ) { (function () { ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.ident"] ); })()).push(v_m2); })() } else { (function () { (function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.ident"]  = [v_m2]); })(); })() };return(1) })() } else { return (function () { return(false) })() } })())}))) })())) })())) })())) ? false : true)) ) { (function () { (v_MATCH.v_to = v_last_pos); })() };return(1) })())}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Expression.f_capture_name;  // v8 bug workaround
  // method hyper_op
  Perlito$Expression.f_hyper_op = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((((function () { var v_last_pos = null;
(v_last_pos = v_MATCH.f_to());if ( f_bool(( f_bool(((function () { return((f_and((">>" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))}))) })())) ? false : true)) ) { (function () { (v_MATCH.v_to = v_last_pos); })() };return(1) })()))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Expression.f_hyper_op;  // v8 bug workaround
  // method operator
  Perlito$Expression.f_operator = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(f_or(((function () { return((f_and(f_and(f_and((f_and((".(" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_paren_parse(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["paren_parse"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and((")" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["postfix_or_term", ".( )", f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["paren_parse"] ); })())])) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and((".[" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_square_parse(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["square_parse"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("]" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["postfix_or_term", ".[ ]", f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["square_parse"] ); })())])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and((".{" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_curly_parse(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["curly_parse"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("}" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["postfix_or_term", ".{ }", f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["curly_parse"] ); })())])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("(" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_paren_parse(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["paren_parse"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and((")" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["postfix_or_term", "( )", f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["paren_parse"] ); })())])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("[" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_square_parse(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["square_parse"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("]" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["postfix_or_term", "[ ]", f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["square_parse"] ); })())])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("->" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))})), function () { return ((function () { var v_last_pos = null;
(v_last_pos = v_MATCH.f_to());if ( f_bool(( f_bool(((function () { return(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())) })())) ? false : true)) ) { (function () { (v_MATCH.v_to = v_last_pos); })() };return(1) })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_list_parse(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["list_parse"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { var v_block = null;
(v_block = (function () { if ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["list_parse"] ); })())) == null) { (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["list_parse"] ); })())) = {} }; return ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["list_parse"] ); })()))["end_block"] ); })());if ( f_bool(v_block.f_sig()) ) { (function () { f_die((f_string("Signature error in block"))); })() };return((v_MATCH.v_capture = ["postfix_or_term", "block", v_block.f_stmts(), (function () { if ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["list_parse"] ); })())) == null) { (f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["list_parse"] ); })())) = {} }; return ((f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["list_parse"] ); })()))["exp"] ); })()])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and(f_and(f_and((f_and(("{" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_last_pos = null;
(v_last_pos = v_MATCH.f_to());if ( f_bool(( f_bool(((function () { return(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())) })())) ? false : true)) ) { (function () { (v_MATCH.v_to = v_last_pos); })() };return(1) })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_exp_stmts(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.exp_stmts"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_last_pos = null;
(v_last_pos = v_MATCH.f_to());if ( f_bool(( f_bool(((function () { return(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())) })())) ? false : true)) ) { (function () { (v_MATCH.v_to = v_last_pos); })() };return(1) })())}), function () { return (f_and(("}" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["postfix_or_term", "block", f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.exp_stmts"] ); })())])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("method" == (v_str || "").substr(v_MATCH.f_to(), 6)), function () { return ((v_MATCH.v_to = f_add(6, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_method_def(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.method_def"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["term", f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.method_def"] ); })())])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("sub" == (v_str || "").substr(v_MATCH.f_to(), 3)), function () { return ((v_MATCH.v_to = f_add(3, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_sub_def(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.sub_def"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["term", f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.sub_def"] ); })())])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("token" == (v_str || "").substr(v_MATCH.f_to(), 5)), function () { return ((v_MATCH.v_to = f_add(5, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_token(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.token"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["term", f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.token"] ); })())])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("do" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_statement_parse(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["statement_parse"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["term", (function () { var tmp = {v_block: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["statement_parse"] ); })())}; tmp.__proto__ = Do; return tmp })()])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("??" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_ternary_parse(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["ternary_parse"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and(("!!" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["op", "?? !!", f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["ternary_parse"] ); })())])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_var_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.var_ident"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["term", f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.var_ident"] ); })())])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("$<" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_capture_name(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["capture_name"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_and((">" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["term", (function () { var tmp = {v_obj: (function () { var tmp = {v_sigil: "$",v_twigil: "",v_name: "/"}; tmp.__proto__ = Var; return tmp })(),v_index_exp: (function () { var tmp = {v_buf: ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["capture_name"] ); })()).f_string()}; tmp.__proto__ = Val$Buf; return tmp })()}; tmp.__proto__ = Lookup; return tmp })()])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Precedence.f_op_parse(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Precedence.op_parse"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Precedence.op_parse"] ); })()))) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.ident"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(((function () { var v_last_pos = null;
(v_last_pos = v_MATCH.f_to());if ( f_bool(( f_bool(((function () { return(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())) })())) ? false : true)) ) { (function () { (v_MATCH.v_to = v_last_pos); })() };return(1) })()), function () { return (f_and(("=>" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))}))}))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? true : false));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["term", (function () { var tmp = {v_buf: ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.ident"] ); })()).f_string()}; tmp.__proto__ = Val$Buf; return tmp })()])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and((f_and(("True" == (v_str || "").substr(v_MATCH.f_to(), 4)), function () { return ((v_MATCH.v_to = f_add(4, v_MATCH.f_to())))})), function () { return ((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_word(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("(" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())})) })()))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? false : true));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["term", (function () { var tmp = {v_bit: 1}; tmp.__proto__ = Val$Bit; return tmp })()])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and((f_and(("False" == (v_str || "").substr(v_MATCH.f_to(), 5)), function () { return ((v_MATCH.v_to = f_add(5, v_MATCH.f_to())))})), function () { return ((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_word(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("(" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())})) })()))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? false : true));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["term", (function () { var tmp = {v_bit: 0}; tmp.__proto__ = Val$Bit; return tmp })()])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and((f_and(("and" == (v_str || "").substr(v_MATCH.f_to(), 3)), function () { return ((v_MATCH.v_to = f_add(3, v_MATCH.f_to())))})), function () { return ((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_word(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("(" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())})) })()))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? false : true));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["op", "and"])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and((f_and(("not" == (v_str || "").substr(v_MATCH.f_to(), 3)), function () { return ((v_MATCH.v_to = f_add(3, v_MATCH.f_to())))})), function () { return ((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_word(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("(" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())})) })()))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? false : true));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["op", "not"])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and(f_and(f_and((f_and(("use" == (v_str || "").substr(v_MATCH.f_to(), 3)), function () { return ((v_MATCH.v_to = f_add(3, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_full_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.full_ident"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_last_pos = null;
(v_last_pos = v_MATCH.f_to());if ( f_bool(( f_bool(((function () { return(((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and((f_and(("-" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());if ( f_bool((v_MATCH).hasOwnProperty("Perlito::Grammar.ident")) ) { (function () { ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.ident"] ); })()).push(v_m2); })() } else { (function () { (function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.ident"]  = [v_m2]); })(); })() };return(1) })() } else { return (function () { return(false) })() } })())}))) })())) })())) })())) ? false : true)) ) { (function () { (v_MATCH.v_to = v_last_pos); })() };return(1) })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_list_parse(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["list_parse"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["term", (function () { var tmp = {v_mod: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.full_ident"] ); })())}; tmp.__proto__ = Use; return tmp })()])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and(((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(f_or(((function () { return(((f_and(("package" == (v_str || "").substr(v_MATCH.f_to(), 7)), function () { return ((v_MATCH.v_to = f_add(7, v_MATCH.f_to())))})))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("class" == (v_str || "").substr(v_MATCH.f_to(), 5)), function () { return ((v_MATCH.v_to = f_add(5, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("grammar" == (v_str || "").substr(v_MATCH.f_to(), 7)), function () { return ((v_MATCH.v_to = f_add(7, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("role" == (v_str || "").substr(v_MATCH.f_to(), 4)), function () { return ((v_MATCH.v_to = f_add(4, v_MATCH.f_to())))})))) })())})) })()), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_grammar(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.grammar"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["term", f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.grammar"] ); })())])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and(f_and(f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_declarator(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.declarator"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_opt_type(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.opt_type"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_opt_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_var_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.var_ident"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["term", (function () { var tmp = {v_decl: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.declarator"] ); })()),v_type: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.opt_type"] ); })()),v_var: f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.var_ident"] ); })())}; tmp.__proto__ = Decl; return tmp })()])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(f_and((f_and(("." == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_hyper_op(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["hyper_op"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.ident"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return((f_and(f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_list_parse(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["list_parse"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["postfix_or_term", "methcall", ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.ident"] ); })()).f_string(), f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["list_parse"] ); })()), f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["hyper_op"] ); })())])) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_or(((function () { return((v_MATCH.v_capture = ["postfix_or_term", "methcall_no_params", ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.ident"] ); })()).f_string(), f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["hyper_op"] ); })())])) })()), function () { return 1})))) })())})) })())}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_optional_namespace_before_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.optional_namespace_before_ident"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_ident(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.ident"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(((function () { return((f_and(f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_list_parse(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["list_parse"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })())}), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["postfix_or_term", "funcall", ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.optional_namespace_before_ident"] ); })()).f_string(), ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.ident"] ); })()).f_string(), f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["list_parse"] ); })())])) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return(((f_and(("." == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? true : false));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })()), function () { return (f_or(((function () { var v_namespace = null;
var v_name = null;
(v_namespace = ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.optional_namespace_before_ident"] ); })()).f_string());(v_name = ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.ident"] ); })()).f_string());if ( f_bool(v_namespace) ) { (function () { (v_name = (f_string(v_namespace) + f_string("::") + f_string(v_name))); })() };return((v_MATCH.v_capture = ["term", (function () { var tmp = {v_name: v_name}; tmp.__proto__ = Proto; return tmp })()])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_or(((function () { return((v_MATCH.v_capture = ["postfix_or_term", "funcall_no_params", ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.optional_namespace_before_ident"] ); })()).f_string(), ((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.ident"] ); })()).f_string()])) })()), function () { return 1})))) })())})) })())}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_val_num(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.val_num"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["term", f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.val_num"] ); })())])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_val_int(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.val_int"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["term", f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.val_int"] ); })())])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_val_buf(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.val_buf"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["term", f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.val_buf"] ); })())])) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = ["space", " "])) })()), function () { return 1}))}))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Expression.f_operator;  // v8 bug workaround
  // method has_newline_after
  Perlito$Expression.f_has_newline_after = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(((function () { return(((f_and(("#" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_is_newline(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_space(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_m2 = null;
(v_m2 = v_grammar.f_has_newline_after(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })())}))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Expression.f_has_newline_after;  // v8 bug workaround
  // method has_no_comma_or_colon_after
  Perlito$Expression.f_has_no_comma_or_colon_after = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((f_and(f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_ws(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());return(1) })() } else { return (function () { return(false) })() } })()), function () { return ((function () { var v_tmp = null;
(v_tmp = v_MATCH);(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_tmp.f_to(),v_to: v_tmp.f_to(),v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = ((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(((function () { return((((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(((function () { return(((f_and(("," == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and((":" == (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))})))) })())})) })()))) })())) })()));(v_tmp.v_bool = ( f_bool(v_MATCH) ? false : true));(v_MATCH = v_tmp);return(( f_bool(v_MATCH) ? true : false)) })())}), function () { return (f_and(("" != (v_str || "").substr(v_MATCH.f_to(), 1)), function () { return ((v_MATCH.v_to = f_add(1, v_MATCH.f_to())))}))}))) })())) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Expression.f_has_no_comma_or_colon_after;  // v8 bug workaround
  // method list_parse
  Perlito$Expression.f_list_parse = function (v_str, v_pos) {
    var v_self = this;
    try { var v_expr = null;
var v_last_pos = null;
var v_is_first_token = null;
var v_lexer_stack = null;
var v_terminated = null;
var v_last_token_was_space = null;
var v_get_token = null;
var v_prec = null;
var v_res = null;
var v_block = null;
var v_result = null;
(v_last_pos = v_pos);(v_is_first_token = true);(v_lexer_stack = []);(v_terminated = 0);(v_last_token_was_space = 1);(v_get_token = function () { try { var v_v = null;
if ( f_bool(f_elems(v_lexer_stack)) ) { (function () { (v_v = v_lexer_stack.pop());if ( f_bool(f_and(f_and(v_is_first_token, function () { return ((v_v[0] == "op"))}), function () { return ( f_bool((Perlito$Precedence.f_is_fixity_type("prefix", v_v[1]))) ? false : true)})) ) { (function () { (v_v[0] = "end"); })() }; })() } else { (function () { var v_m = null;
(v_m = v_self.f_operator(v_str, v_last_pos));if ( f_bool(( f_bool(v_m) ? false : true)) ) { (function () { throw(["end", "*end*"]); })() };(v_v = f_scalar(v_m));if ( f_bool(f_and(f_and(v_is_first_token, function () { return ((v_v[0] == "op"))}), function () { return ( f_bool((Perlito$Precedence.f_is_fixity_type("prefix", v_v[1]))) ? false : true)})) ) { (function () { (v_v[0] = "end"); })() };if ( f_bool((v_v[0] != "end")) ) { (function () { (v_last_pos = v_m.f_to()); })() }; })() };if ( f_bool(f_and(f_and((((v_v[0]) == "postfix_or_term")), function () { return (((v_v[1]) == "block"))}), function () { return v_last_token_was_space})) ) { (function () { if ( f_bool(v_self.f_has_newline_after(v_str, v_last_pos)) ) { (function () { (v_terminated = 1);v_lexer_stack.push(["end", "*end*"]); })() } else { (function () { if ( f_bool(v_self.f_has_no_comma_or_colon_after(v_str, v_last_pos)) ) { (function () { (v_terminated = 1);v_lexer_stack.push(["end", "*end*"]); })() }; })() }; })() };(v_last_token_was_space = ((v_v[0] == "space")));(v_is_first_token = false);throw(v_v) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } }  });(v_prec = (function () { var tmp = {v_get_token: v_get_token,v_reduce: v_reduce_to_ast,v_end_token: ["and", "or", "!!", "]", ")", "}", ";", "if", "else", "elsif", "unless", "when", "for", "while", "loop"]}; tmp.__proto__ = Perlito$Precedence; return tmp })());(v_res = v_prec.f_precedence_parse());if ( f_bool((f_elems(v_res) == 0)) ) { (function () { throw((function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_last_pos,v_bool: 1,v_capture: (function () { var a = {}; a["exp"] = "*undef*"; a["end_block"] = null; a["terminated"] = null; return a })()}; tmp.__proto__ = Perlito$Match; return tmp })()); })() };if ( f_bool((f_elems(v_res) > 1)) ) { (function () { (v_block = v_res.pop());(v_block = (function () { var tmp = {v_stmts: v_block[2],v_sig: v_block[3]}; tmp.__proto__ = Lit$Block; return tmp })()); })() };(v_result = v__NAMESPACE.f_pop_term(v_res));if ( f_bool((f_elems(v_res) > 0)) ) { (function () { (v_block = v_res.pop());(v_block = (function () { var tmp = {v_stmts: v_block[2],v_sig: v_block[3]}; tmp.__proto__ = Lit$Block; return tmp })()); })() };throw((function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_last_pos,v_bool: 1,v_capture: (function () { var a = {}; a["exp"] = v_result; a["end_block"] = v_block; a["terminated"] = v_terminated; return a })()}; tmp.__proto__ = Perlito$Match; return tmp })()) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Expression.f_list_parse;  // v8 bug workaround
  // method circumfix_parse
  Perlito$Expression.f_circumfix_parse = function (v_str, v_pos, v_delimiter) {
    var v_self = this;
    try { var v_expr = null;
var v_last_pos = null;
var v_get_token = null;
var v_prec = null;
var v_res = null;
(v_last_pos = v_pos);(v_get_token = function () { try { var v_m = null;
var v_v = null;
(v_m = v_self.f_operator(v_str, v_last_pos));if ( f_bool(( f_bool(v_m) ? false : true)) ) { (function () { f_die((f_string("Expected closing delimiter: ")), ((v_delimiter)), " near ", v_last_pos); })() };(v_v = f_scalar(v_m));if ( f_bool((v_v[0] != "end")) ) { (function () { (v_last_pos = v_m.f_to()); })() };throw(v_v) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } }  });(v_prec = (function () { var tmp = {v_get_token: v_get_token,v_reduce: v_reduce_to_ast,v_end_token: v_delimiter}; tmp.__proto__ = Perlito$Precedence; return tmp })());(v_res = v_prec.f_precedence_parse());(v_res = v__NAMESPACE.f_pop_term(v_res));if ( f_bool(( f_bool(((v_res != null))) ? false : true)) ) { (function () { (v_res = "*undef*"); })() };throw((function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_last_pos,v_bool: 1,v_capture: v_res}; tmp.__proto__ = Perlito$Match; return tmp })()) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Expression.f_circumfix_parse;  // v8 bug workaround
  // method ternary_parse
  Perlito$Expression.f_ternary_parse = function (v_str, v_pos) {
    var v_self = this;
    try { throw(v_self.f_circumfix_parse(v_str, v_pos, ["!!"])) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Expression.f_ternary_parse;  // v8 bug workaround
  // method curly_parse
  Perlito$Expression.f_curly_parse = function (v_str, v_pos) {
    var v_self = this;
    try { throw(v_self.f_circumfix_parse(v_str, v_pos, ["}"])) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Expression.f_curly_parse;  // v8 bug workaround
  // method square_parse
  Perlito$Expression.f_square_parse = function (v_str, v_pos) {
    var v_self = this;
    try { throw(v_self.f_circumfix_parse(v_str, v_pos, ["]"])) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Expression.f_square_parse;  // v8 bug workaround
  // method paren_parse
  Perlito$Expression.f_paren_parse = function (v_str, v_pos) {
    var v_self = this;
    try { throw(v_self.f_circumfix_parse(v_str, v_pos, [")"])) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Expression.f_paren_parse;  // v8 bug workaround
  // method exp_parse
  Perlito$Expression.f_exp_parse = function (v_str, v_pos) {
    var v_self = this;
    try { var v_expr = null;
var v_last_pos = null;
var v_lexer_stack = null;
var v_terminated = null;
var v_get_token = null;
var v_prec = null;
var v_res = null;
var v_block = null;
var v_result = null;
(v_last_pos = v_pos);(v_lexer_stack = []);(v_terminated = 0);(v_get_token = function () { try { var v_v = null;
if ( f_bool(f_elems(v_lexer_stack)) ) { (function () { (v_v = v_lexer_stack.pop()); })() } else { (function () { var v_m = null;
(v_m = v_self.f_operator(v_str, v_last_pos));if ( f_bool(( f_bool(v_m) ? false : true)) ) { (function () { throw(["end", "*end*"]); })() };(v_v = f_scalar(v_m));if ( f_bool((v_v[0] != "end")) ) { (function () { (v_last_pos = v_m.f_to()); })() }; })() };if ( f_bool(f_or(f_or(f_or(f_or((f_and((((v_v[0]) == "postfix_or_term")), function () { return (((v_v[1]) == "block"))})), function () { return (f_and((((v_v[0]) == "term")), function () { return (f_isa((v_v[1]), "Sub"))}))}), function () { return (f_and((((v_v[0]) == "term")), function () { return (f_isa((v_v[1]), "Method"))}))}), function () { return (f_and((((v_v[0]) == "term")), function () { return (f_isa((v_v[1]), "Do"))}))}), function () { return (f_and((((v_v[0]) == "term")), function () { return (f_isa((v_v[1]), "CompUnit"))}))})) ) { (function () { if ( f_bool(v_self.f_has_newline_after(v_str, v_last_pos)) ) { (function () { (v_terminated = 1);v_lexer_stack.push(["end", "*end*"]); })() }; })() };throw(v_v) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } }  });(v_prec = (function () { var tmp = {v_get_token: v_get_token,v_reduce: v_reduce_to_ast,v_end_token: ["]", ")", "}", ";", "if", "else", "elsif", "unless", "when", "for", "while", "loop"]}; tmp.__proto__ = Perlito$Precedence; return tmp })());(v_res = v_prec.f_precedence_parse());if ( f_bool((f_elems(v_res) == 0)) ) { (function () { throw((function () { var tmp = {v_bool: 0}; tmp.__proto__ = Perlito$Match; return tmp })()); })() };if ( f_bool((f_elems(v_res) > 1)) ) { (function () { (v_block = v_res.pop());(v_block = (function () { var tmp = {v_stmts: v_block[2],v_sig: v_block[3]}; tmp.__proto__ = Lit$Block; return tmp })()); })() };(v_result = v__NAMESPACE.f_pop_term(v_res));if ( f_bool((f_elems(v_res) > 0)) ) { (function () { (v_block = v_res.pop());if ( f_bool(( f_bool((f_isa(v_block, "Lit::Block"))) ? false : true)) ) { (function () { (v_block = (function () { var tmp = {v_stmts: v_block[2],v_sig: v_block[3]}; tmp.__proto__ = Lit$Block; return tmp })()); })() }; })() };throw((function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_last_pos,v_bool: 1,v_capture: (function () { var a = {}; a["exp"] = v_result; a["end_block"] = v_block; a["terminated"] = v_terminated; return a })()}; tmp.__proto__ = Perlito$Match; return tmp })()) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Expression.f_exp_parse;  // v8 bug workaround
  // method exp_stmt
  Perlito$Expression.f_exp_stmt = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(f_or(f_or(f_or(((function () { return((f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_if(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.if"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.if"] ); })()))) })()), function () { return 1}))}))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_unless(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.unless"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.unless"] ); })()))) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_when(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.when"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.when"] ); })()))) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_for(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.for"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.for"] ); })()))) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_while(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.while"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.while"] ); })()))) })()), function () { return 1}))}))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return((f_and(((function () { var v_m2 = null;
(v_m2 = Perlito$Grammar.f_loop(v_str, v_MATCH.f_to()));if ( f_bool(v_m2) ) { return (function () { (v_MATCH.v_to = v_m2.f_to());(function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.loop"]  = v_m2); })();return(1) })() } else { return (function () { return(false) })() } })()), function () { return (f_or(((function () { return((v_MATCH.v_capture = f_scalar((function () { if (v_MATCH == null) { v_MATCH = {} }; return (v_MATCH["Perlito::Grammar.loop"] ); })()))) })()), function () { return 1}))}))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Expression.f_exp_stmt;  // v8 bug workaround
  // method statement_modifier
  Perlito$Expression.f_statement_modifier = function (v_str, v_pos) {
    var v_grammar = this;
    try { var v_MATCH = null;
(v_MATCH = (function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_pos,v_bool: 1}; tmp.__proto__ = Perlito$Match; return tmp })());(v_MATCH.v_bool = (((function () { var v_pos1 = null;
(v_pos1 = v_MATCH.f_to());return(f_or(f_or(f_or(f_or(f_or(((function () { return(((f_and(("if" == (v_str || "").substr(v_MATCH.f_to(), 2)), function () { return ((v_MATCH.v_to = f_add(2, v_MATCH.f_to())))})))) })()), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("unless" == (v_str || "").substr(v_MATCH.f_to(), 6)), function () { return ((v_MATCH.v_to = f_add(6, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("when" == (v_str || "").substr(v_MATCH.f_to(), 4)), function () { return ((v_MATCH.v_to = f_add(4, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("for" == (v_str || "").substr(v_MATCH.f_to(), 3)), function () { return ((v_MATCH.v_to = f_add(3, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("while" == (v_str || "").substr(v_MATCH.f_to(), 5)), function () { return ((v_MATCH.v_to = f_add(5, v_MATCH.f_to())))})))) })())}), function () { return ((function () { (v_MATCH.v_to = v_pos1);return(((f_and(("loop" == (v_str || "").substr(v_MATCH.f_to(), 4)), function () { return ((v_MATCH.v_to = f_add(4, v_MATCH.f_to())))})))) })())})) })())));return(v_MATCH) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Expression.f_statement_modifier;  // v8 bug workaround
  // method statement_parse
  Perlito$Expression.f_statement_parse = function (v_str, v_pos) {
    var v_self = this;
    try { var v_expr = null;
var v_last_pos = null;
var v_lexer_stack = null;
var v_spaces = null;
var v_res = null;
var v_modifier = null;
var v_modifier_exp = null;
(v_last_pos = v_pos);(v_lexer_stack = []);(v_spaces = Perlito$Grammar.f_ws(v_str, v_pos));if ( f_bool(v_spaces) ) { (function () { (v_pos = v_spaces.f_to()); })() };(v_res = v_self.f_exp_stmt(v_str, v_pos));if ( f_bool(v_res) ) { (function () { throw(v_res); })() };(v_res = v_self.f_exp_parse(v_str, v_pos));if ( f_bool(( f_bool((v_res)) ? false : true)) ) { (function () { throw(v_res); })() };if ( f_bool(f_isa((function () { if ((f_scalar(v_res)) == null) { (f_scalar(v_res)) = {} }; return ((f_scalar(v_res))["exp"] ); })(), "Lit::Block")) ) { (function () { (function () { if ((f_scalar(v_res)) == null) { (f_scalar(v_res)) = {} }; return ((f_scalar(v_res))["exp"]  = (function () { var tmp = {v_block: (function () { if ((f_scalar(v_res)) == null) { (f_scalar(v_res)) = {} }; return ((f_scalar(v_res))["exp"] ); })()}; tmp.__proto__ = Do; return tmp })()); })(); })() };if ( f_bool((function () { if ((f_scalar(v_res)) == null) { (f_scalar(v_res)) = {} }; return ((f_scalar(v_res))["end_block"] ); })()) ) { (function () { f_die((f_string("Unexpected block after expression near ")), v_pos); })() };if ( f_bool((function () { if ((f_scalar(v_res)) == null) { (f_scalar(v_res)) = {} }; return ((f_scalar(v_res))["terminated"] ); })()) ) { (function () { (v_res.v_capture = (function () { if ((f_scalar(v_res)) == null) { (f_scalar(v_res)) = {} }; return ((f_scalar(v_res))["exp"] ); })());throw(v_res); })() };(v_modifier = v_self.f_statement_modifier(v_str, v_res.f_to()));if ( f_bool(( f_bool((v_modifier)) ? false : true)) ) { (function () { (v_res.v_capture = (function () { if ((f_scalar(v_res)) == null) { (f_scalar(v_res)) = {} }; return ((f_scalar(v_res))["exp"] ); })());throw(v_res); })() };(v_modifier_exp = v_self.f_exp_parse(v_str, v_modifier.f_to()));if ( f_bool(( f_bool((v_modifier_exp)) ? false : true)) ) { (function () { f_die((f_string("Expected expression after '")), v_modifier, (f_string("'"))); })() };if ( f_bool((function () { if ((f_scalar(v_modifier_exp)) == null) { (f_scalar(v_modifier_exp)) = {} }; return ((f_scalar(v_modifier_exp))["end_block"] ); })()) ) { (function () { f_die((f_string("Unexpected block after expression near ")), v_modifier.f_to()); })() };throw((function () { var tmp = {v_str: v_str,v_from: v_pos,v_to: v_modifier_exp.f_to(),v_bool: 1,v_capture: (function () { var a = {}; a["exp"] = (function () { if ((f_scalar(v_res)) == null) { (f_scalar(v_res)) = {} }; return ((f_scalar(v_res))["exp"] ); })(); a["modifier"] = (v_modifier).f_string(); a["modifier_exp"] = (function () { if ((f_scalar(v_modifier_exp)) == null) { (f_scalar(v_modifier_exp)) = {} }; return ((f_scalar(v_modifier_exp))["exp"] ); })(); return a })()}; tmp.__proto__ = Perlito$Match; return tmp })()) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
  Perlito$Expression.f_statement_parse;  // v8 bug workaround
// use Perlito::Precedence
;// use Perlito::Grammar
;// use Perlito::Perl5::Emitter
;(v_reduce_to_ast = function (v_op_stack, v_num_stack) { try { var v_last_op = null;
(v_last_op = v_op_stack.shift());if ( f_bool((v_last_op[0] == "prefix")) ) { return (function () { return(f_push(v_num_stack, (function () { var tmp = {v_namespace: "",v_code: (f_string("prefix:<") + f_string(v_last_op[1]) + f_string(">")),v_arguments: [v__NAMESPACE.f_pop_term(v_num_stack)]}; tmp.__proto__ = Apply; return tmp })())) })() } else { return (function () { if ( f_bool((v_last_op[0] == "postfix")) ) { return (function () { return(f_push(v_num_stack, (function () { var tmp = {v_namespace: "",v_code: (f_string("postfix:<") + f_string(v_last_op[1]) + f_string(">")),v_arguments: [v__NAMESPACE.f_pop_term(v_num_stack)]}; tmp.__proto__ = Apply; return tmp })())) })() } else { return (function () { if ( f_bool((v_last_op[0] == "postfix_or_term")) ) { return (function () { return(v_num_stack.push(v__NAMESPACE.f_reduce_postfix(v_last_op, v__NAMESPACE.f_pop_term(v_num_stack)))) })() } else { return (function () { if ( f_bool(Perlito$Precedence.f_is_assoc_type("list", v_last_op[1])) ) { return (function () { var v_arg = null;
if ( f_bool((f_elems(v_num_stack) < 2)) ) { (function () { var v_v2 = null;
(v_v2 = v__NAMESPACE.f_pop_term(v_num_stack));if ( f_bool(f_and((f_isa(v_v2, "Apply")), function () { return ((v_v2.f_code() == ((f_string("list:<") + f_string(v_last_op[1]) + f_string(">")))))})) ) { (function () { (v_v2.f_arguments()).push(null);v_num_stack.push(v_v2); })() } else { (function () { f_push(v_num_stack, (function () { var tmp = {v_namespace: "",v_code: (f_string("list:<") + f_string(v_last_op[1]) + f_string(">")),v_arguments: [v_v2, null]}; tmp.__proto__ = Apply; return tmp })()); })() };throw(null); })() } else { (function () { var v_v2 = null;
(v_v2 = v__NAMESPACE.f_pop_term(v_num_stack));(v_arg = [v__NAMESPACE.f_pop_term(v_num_stack), v_v2]); })() };if ( f_bool(f_and(f_and((f_isa((v_arg[0]), "Apply")), function () { return ((v_last_op[0] == "infix"))}), function () { return (((v_arg[0]).f_code() == ((f_string("list:<") + f_string(v_last_op[1]) + f_string(">")))))})) ) { (function () { f_push(v_num_stack, (function () { var tmp = {v_namespace: "",v_code: (v_arg[0]).f_code(),v_arguments: (function () { var a = []; (function(a_) { for (var i_ = 0; i_ < a_.length ; i_++) { a.push(a_[i_]) }})((((v_arg[0]).f_arguments()))); a.push(v_arg[1]);  return a })()}; tmp.__proto__ = Apply; return tmp })());throw(null); })() };return(f_push(v_num_stack, (function () { var tmp = {v_namespace: "",v_code: (f_string("list:<") + f_string(v_last_op[1]) + f_string(">")),v_arguments: v_arg}; tmp.__proto__ = Apply; return tmp })())) })() } else { return (function () { if ( f_bool(Perlito$Precedence.f_is_assoc_type("chain", v_last_op[1])) ) { return (function () { var v_v2 = null;
var v_arg = null;
if ( f_bool((f_elems(v_num_stack) < 2)) ) { (function () { f_die((f_string("Missing value after operator ") + f_string(v_last_op[1]))); })() };(v_v2 = v__NAMESPACE.f_pop_term(v_num_stack));(v_arg = [v__NAMESPACE.f_pop_term(v_num_stack), v_v2]);return(f_push(v_num_stack, (function () { var tmp = {v_namespace: "",v_code: (f_string("infix:<") + f_string(v_last_op[1]) + f_string(">")),v_arguments: v_arg}; tmp.__proto__ = Apply; return tmp })())) })() } else { return (function () { if ( f_bool((v_last_op[0] == "ternary")) ) { return (function () { var v_v2 = null;
if ( f_bool(((f_elems(v_num_stack) < 2))) ) { (function () { f_die((f_string("Missing value after ternary operator"))); })() };(v_v2 = v__NAMESPACE.f_pop_term(v_num_stack));return(f_push(v_num_stack, (function () { var tmp = {v_namespace: "",v_code: (f_string("ternary:<") + f_string(v_last_op[1]) + f_string(">")),v_arguments: [v__NAMESPACE.f_pop_term(v_num_stack), v_last_op[2], v_v2]}; tmp.__proto__ = Apply; return tmp })())) })() } else { return (function () { var v_v2 = null;
if ( f_bool(((f_elems(v_num_stack) < 2))) ) { (function () { f_die((f_string("missing value after operator '") + f_string(v_last_op[1]) + f_string((f_string("'"))))); })() };(v_v2 = v__NAMESPACE.f_pop_term(v_num_stack));return(f_push(v_num_stack, (function () { var tmp = {v_namespace: "",v_code: (f_string("infix:<") + f_string(v_last_op[1]) + f_string(">")),v_arguments: [v__NAMESPACE.f_pop_term(v_num_stack), v_v2]}; tmp.__proto__ = Apply; return tmp })())) })() } })() } })() } })() } })() } })() } } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } }  });})();
;})();

// class GLOBAL
if (typeof GLOBAL != 'object') {
  GLOBAL = function() {};
  GLOBAL = new GLOBAL;
  GLOBAL.f_isa = function (s) { return s == 'GLOBAL' };
  GLOBAL.f_perl = function () { return 'GLOBAL.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = GLOBAL;
// class Perlito
if (typeof Perlito != 'object') {
  Perlito = function() {};
  Perlito = new Perlito;
  Perlito.f_isa = function (s) { return s == 'Perlito' };
  Perlito.f_perl = function () { return 'Perlito.new(' + Main._dump(this) + ')' };
}
(function () {
  var v__NAMESPACE = Perlito;
  // sub compile_p6_to_js
  Perlito.f_compile_p6_to_js = function (v_s) {
    try { var v_ast = null;
(v_ast = Perlito$Grammar.f_exp_stmts(v_s, 0));return((function () { var tmp = {v_name: "GLOBAL",v_body: f_scalar(v_ast)}; tmp.__proto__ = CompUnit; return tmp })().f_emit_javascript()) } catch(err) { if ( err instanceof Error ) { throw(err) } else { return(err) } } 
  }
// use Perlito::Javascript::Emitter
;// use Perlito::Grammar
;// use Perlito::Grammar::Control
;// use Perlito::Grammar::Regex
;// use Perlito::Emitter::Token
;// use Perlito::Precedence
;// use Perlito::Expression
;})();
;})();

