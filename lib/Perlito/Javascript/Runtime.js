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
// Copyright 2009, 2010, 2011 by Flavio Soibelmann Glock and others.
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
    if (typeof read == 'function' ) {
        // v8
        return read(filename);
    }
    f_die("IO.slurp() not implemented");    
}

// class Main
if (typeof Main != 'object') {
  Main = function() {};
  Main = new Main;
}
(function () {
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
      else if ( i.match( /^f_/ ) ) {
      }
      else {
        out.push( i + " => " + f_perl(o[i]) ) 
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
        return true;
    }
}
if (typeof f_say != 'function') {
  f_say = function () {
    var i;
    for (i = 0; i < f_say.arguments.length; i++) {
        f_print( f_say.arguments[i] );
    }
    return f_print("\n");
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
    return out;
  }
  for(var i in o) { 
    out.push(i) 
  }
  return out;
}
f_pairs = function (o) {
  if ( o == null ) { return [] };
  if ( typeof o.f_pairs == 'function' ) { return o.f_pairs() }
  if ( typeof o == 'object' && (o instanceof Array) ) {
    var count = 0;
    for(var i in o) { 
      var tmp = {v_key: count,v_value: i}; 
      tmp.__proto__ = Pair; 
      out.push(tmp) 
      count++;
    }
    return o;
  }
  var out = [];
  for(var i in o) { 
    var tmp = {v_key: i,v_value: o[i]}; 
    tmp.__proto__ = Pair; 
    out.push(tmp) 
  }
  return out;
}
var _id = 0;
f_id = function (o) {
  if ( o == null ) { return '_id_' + 'Mu' };
  if ( typeof o.f_id == 'function' ) { 
    return o.f_id() 
  }
  if ( o._id ) { 
      return o._id
  }
  switch (typeof o){
    case "string":   return '_id_str_' + o;
    case "number":   return '_id_num_' + o;
    case "boolean":  return '_id_bool_' + o;
  }
  o._id = ++_id;
  return o._id;
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
  for(var i in o) { 
    return true
  }
  return false
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

