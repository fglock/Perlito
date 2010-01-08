// lib/MiniPerl6/Javascript/Runtime.go
//
// Runtime for "Perlito" MiniPerl6-in-Go
//
// AUTHORS
//
// Flavio Soibelmann Glock  fglock@gmail.com
//
// SEE ALSO
//
// The Perl 6 homepage at http://dev.perl.org/perl6
// The Pugs homepage at http://pugscode.org/
//
// COPYRIGHT
//
// Copyright 2009 by Flavio Soibelmann Glock and others.
//
// This program is free software; you can redistribute it and/or modify it
// under the same terms as Perl itself.
//
// See http://www.perl.com/perl/misc/Artistic.html

package main

import (
	"fmt";
	"strconv";
	"os";
	"strings";
	"runtime";
	"unicode";
    // "reflect";
)

// interfaces used by the runtime

type Any interface{}

type join_er interface {
	f_join(v Capture) *Any;
}
type perl_er interface {
	f_perl(v Capture) *Any;
}
type scalar_er interface {
	f_scalar(v Capture) *Any;
}
type isa_er interface {
	f_isa(v Capture) *Any;
}
type values_er interface {
	f_values(v Capture) *Any;
}
type int_er interface {
	f_int(v Capture) *Any;
}
type str_er interface {
	f_str(v Capture) *Any;
}
type bool_er interface {
	f_bool(v Capture) *Any;
}
type array_er interface {
	f_array(v Capture) *Any;
}
type hash_er interface {
	f_hash(v Capture) *Any;
}
type push_er interface {
	f_push(v Capture) *Any;
}
type lookup_er interface {
	f_lookup(v Capture) *Any;
}
type index_er interface {
	f_index(v Capture) *Any;
}

// constants
var i_1  = toInt(1)
var i_0  = toInt(0)
var s_true  = toStr("true")
var s_false = toStr("false")
var s_empty = toStr("")
var s_undef = toStr("undef")

func b_true() *Any {
	var v Any = Bool(true);
	return &v;
}
func b_false() *Any {
	var v Any = Bool(false);
	return &v;
}
func u_undef() *Any {
	var v Any = new(Undef);
	return &v;
}
func a_array() *Any {
    var a = new(Array);
    a.n = -1;
    a.v = make([]*Any, 5);
    var v Any = a; 
    return &v;
}
func h_hash() *Any {
    var h = new(Hash);
    h.h = make(map[string]*Any);
	var v Any = h;
	return &v;
}

type Undef bool

func (i Undef) f_bool(Capture) *Any	{ return b_false() }
func (i Undef) f_int(Capture) *Any	{ return i_0 }
func (i Undef) f_str(Capture) *Any	{ return s_empty }
func (i Undef) f_array(Capture) *Any	{ return a_array() }
func (i Undef) f_hash(Capture) *Any	{ return h_hash() }
func (i Undef) f_perl(Capture) *Any	{ return s_undef }

type Bool bool

func toBool(i bool) *Any {
	var r Any = Bool(i);
	return &r;
}
func tobool(v *Any) bool		{ return bool((*((*v).(bool_er).f_bool(Capture{}))).(Bool)) }
func (i Bool) f_bool(Capture) *Any	{ var v Any = i; return &v }
func (i Bool) f_int(Capture) *Any {
	if i {
		return i_1
	}
	return i_0;
}
func (i Bool) f_str(Capture) *Any {
	if i {
		return s_true
	}
	return s_false;
}
func (i Bool) f_array(Capture) *Any	{ panic("converting bool to array") }
func (i Bool) f_hash(Capture) *Any	{ panic("converting bool to hash") }
func (i Bool) f_not(Capture) *Any {
	if i {
		return b_false()
	}
	return b_true();
}
func (i Bool) f_perl(Capture) *Any	{ return i.f_str(Capture{}) }

type Int int

func toInt(i int) *Any {
	var r Any = Int(i);
	return &r;
}
func toint(v *Any) int	{ return int((*((*v).(int_er).f_int(Capture{}))).(Int)) }
func (i Int) f_bool(Capture) *Any {
	if i == 0 {
		return b_false()
	}
	return b_true();
}
func (i Int) f_int(Capture) *Any	{ var v Any = i; return &v }
func (i Int) f_str(Capture) *Any	{ return toStr(strconv.Itoa(int(i))) }
func (i Int) f_array(Capture) *Any	{ panic("converting int to array") }
func (i Int) f_hash(Capture) *Any	{ panic("converting int to hash") }
func (i Int) f_perl(Capture) *Any	{ return i.f_str(Capture{}) }

type Str string

func toStr(i string) *Any {
	var r Any = Str(i);
	return &r;
}
func tostr(v *Any) string	{ return string((*((*v).(str_er).f_str(Capture{}))).(Str)) }
func (i Str) f_bool(Capture) *Any {
	if i == "" || i == "0" {
		return b_false()
	}
	return b_true();
}
func (i Str) f_int(Capture) *Any {
	n, _ := strconv.Atoi(string(i));
	return toInt(n);
}
func (i Str) f_str(Capture) *Any   { var j Any = i; return &j }
func (i Str) f_array(Capture) *Any	{ panic("converting string to array") }
func (i Str) f_hash(Capture) *Any	{ panic("converting str to hash") }
func (i Str) f_str_equal(v Capture) *Any {
	s2 := tostr(v.p[0]);
	if string(i) != s2 {
		return b_false()
	}
	return b_true();
}
func (i Str) f_perl(Capture) *Any	{ return toStr("\"" + string(i) + "\"") }

type Function func (Capture) *Any

type Hash struct {
	h map[string]*Any;
}

func (i Hash) f_bool(Capture) *Any {
	if len(i.h) == 0 {
		return b_false()
	}
	return b_true();
}
func (i Hash) f_int(Capture) *Any	{ return toInt(len(i.h)) }
func (i Hash) f_str(Capture) *Any	{ return toStr("TODO: hash.Str") }
func (i Hash) f_array(Capture) *Any {
	return a_array()	// TODO
}
func (i *Hash) f_hash(Capture) *Any	{ 
    var p Any;
    var j *Any = &p;
    *j = i;
    return j;
}
func (i *Hash) f_lookup(v Capture) *Any {
	pos := tostr(v.p[0]);
	// TODO laziness
	if i.h == nil {
		i.h = make(map[string]*Any)
	}
	item, found := i.h[pos];
	if !found {
		var item Any;
		i.h[pos] = &item;
		return &item;
	}
	return item;
}
func (i Hash) f_perl(Capture) *Any	{ return toStr("{" + "..." + "}") }
func (i Hash) f_values(v1 Capture) *Any {
	a := a_array();
	for _, value := range i.h {
		(*a).(push_er).f_push(Capture{p: []*Any{value}})
	}
	return a;
}

type Array struct {
	n	int;
	v	[]*Any;
}

func (i Array) f_bool(Capture) *Any {
	if i.n < 0 {
		return b_false()
	}
	return b_true();
}
func (i Array) f_int(Capture) *Any	{ return toInt(i.n + 1) }
func (i Array) f_str(Capture) *Any	{ return i.f_join(Capture{p: []*Any{toStr(" ")}}) }
func (i *Array) f_array(Capture) *Any	{ 
    var v Any = *i; 
    return &v
    // var p Any;
    // var j *Any = &p;
    // *j = i;
    // return j;
}
func (i Array) f_hash(Capture) *Any {
	return h_hash()	// TODO
}
func (i *Array) f_index(v Capture) *Any {
	pos := toint(v.p[0]);
	// TODO autoextend
	// TODO laziness
	if i.v[pos] == nil {
		i.v[pos] = u_undef()	// is this needed?
	}
	return i.v[pos];
}
func (i *Array) f_push(v Capture) *Any {
    i.n++;
    if i.v[i.n] == nil {
        var v Any;
        i.v[i.n] = &v;
    }
    *i.v[i.n] = *v.p[0];
    var v2 Any = i; 
    return &v2
}
func (i *Array) f_pop() *Any {
	i.n--;
	return i.v[i.n + 1];
}
func (i Array) f_join(v1 Capture) *Any {
	var s1 string;
	var sep string;
	if len(v1.p) > 0 {
		sep = tostr(v1.p[0])
	} else {
		sep = ""
	}
	v := i.v;
	if i.n > 0 {
		s1 = tostr(v[0])
	}
	for pos := 1; pos < i.n; pos++ {
		s1 = s1 + sep + tostr(v[pos])
	}
	return toStr(s1);
}
func (i Array) f_perl(v1 Capture) *Any {
	var s1 = i.f_join(Capture{p: []*Any{toStr(", ")}});
	return toStr("[" + tostr(s1) + "]");
}

// Capture is a parameter list, for internal use

type Capture struct {
	p []*Any;
	// invocant *Any;
}

// runtime functions

func f_isa(s Capture) *Any {
	if sc, ok := (*s.p[0]).(isa_er); ok {
		return sc.f_isa(Capture{p: []*Any{s.p[1]}})
	}
	var r Any = b_false;
	return &r;
}
func f_scalar(s Capture) *Any {
	if sc, ok := (*s.p[0]).(scalar_er); ok {
		return sc.f_scalar(Capture{})
	}
	return s.p[0];
}
func f_pop(s Capture) *Any {
	var o = (*s.p[0]).(array_er).f_array(Capture{});
	panic("TODO - Pop");
	return o;
	//return o.f_pop()
}
func f_push(s Capture) *Any {
	var o = (*(*s.p[0]).(array_er).f_array(Capture{})).(push_er).f_push( Capture{ p : []*Any{ s.p[1] } } );
	return o;
}
func f_index(v Capture) *Any {
	var s = tostr(v.p[0]);
	var sep = tostr(v.p[1]);
	return toInt(strings.Index(s, sep));
}
func f_defined(v Capture) *Any {
	switch i := (*v.p[0]).(type) {
	case nil:
		return b_false()
	case Undef:
		return b_false()
	}
	return b_true();
}
func f_die(v Capture) *Any	{ panic(tostr(v.p[0])) }
func f_print(s Capture) *Any {
	for i, _ := range s.p {
        // var t = reflect.Typeof( *s.p[i] );
        // fmt.Println( "type: ", t );
		fmt.Print(tostr(s.p[i]))
	}
	return b_true();
}
func f_print_stderr(s Capture) *Any {
	for i, _ := range s.p {
		fmt.Fprint(os.Stderr, tostr(s.p[i]))
	}
	return b_true();
}
func f_substr(s Capture) *Any {
    var s1 = tostr(s.p[0]);
	var a = toint(s.p[1]);
	var b = toint(s.p[2]);
	// TODO if b < 0
    if a >= len(s1) {
        return toStr("");
    }
	return toStr(s1[a : a+b]);
}
func Go_return(p chan *Any, r *Any) bool {
	p <- r;
	runtime.Goexit();
	return false;
}
func f_and(f1, f2 func() *Any) *Any {
	var tmp = f1();
	if tobool(tmp) {
		return f2()
	}
	return tmp;
}
func f_or(f1, f2 func() *Any) *Any {
	var tmp = f1();
	if tobool(tmp) {
		return tmp
	}
	return f2();
}

// implementation of functions and methods declared in the prelude file

//func (v_self MiniPerl6__Match) f_bool(Capture) *Any {
//    return (*v_self.f_bool(Capture{})).(bool_er).f_bool(Capture{})
//}
func (v_self MiniPerl6__Match) f_int(Capture) *Any {
    return (*v_self.f_string(Capture{})).(int_er).f_int(Capture{})
}
//func (v_self MiniPerl6__Match) f_str(Capture) *Any {
//    return (*v_self.f_string(Capture{})).(str_er).f_str(Capture{})
//}

func Init_Prelude() {

	Method_MiniPerl6__Match.f_scalar = func(v_self *MiniPerl6__Match, v Capture) *Any {
        if v_self.v_bool == nil {
            v_self.v_bool = toBool(false);       
        }
		if tobool(v_self.v_bool) {
            if v_self.v_capture == nil {
                var x Any;
                v_self.v_capture = &x;        
            }
			if tobool(f_defined(Capture{p: []*Any{v_self.v_capture}})) {
				return v_self.v_capture
			}
			return f_substr(Capture{p: []*Any{v_self.v_str, v_self.v_from, toInt(toint(v_self.v_to) - toint(v_self.v_from))}});
		}
		return toStr("");
	};
	Method_MiniPerl6__Match.f_string = func(v_self *MiniPerl6__Match, v Capture) *Any {
        if v_self.v_bool == nil {
            v_self.v_bool = toBool(false);       
        }
		if tobool(v_self.v_bool) {
            if v_self.v_capture == nil {
                var x Any;
                v_self.v_capture = &x;        
            }
			if tobool(f_defined(Capture{p: []*Any{v_self.v_capture}})) {
				return v_self.v_capture
			}
			return f_substr(Capture{p: []*Any{v_self.v_str, v_self.v_from, toInt(toint(v_self.v_to) - toint(v_self.v_from))}});
		}
		return toStr("");
	};

	Method_MiniPerl6__Grammar.f_is_newline = func(v_grammar *MiniPerl6__Grammar, v Capture) *Any {
		var s1 = tostr(v.p[0]);
		var i1 = toint(v.p[1]);
		var b1 = false;
		if (i1+1) < len(s1) && ((s1[i1] == 13 && s1[i1+1] == 10) || (s1[i1] == 10 && s1[i1+1] == 13)) {
			i1 += 2;
			b1 = true;
		} else {
			if s1[i1] == 13 || s1[i1] == 10 {
				i1 += 1;
				b1 = true;
			}
		}
		var m Any = new(MiniPerl6__Match);
		*m.(str_er).f_str(Capture{}) = *v.p[0];
		*m.(from_er).f_from(Capture{}) = *v.p[1];
		*m.(to_er).f_to(Capture{}) = Int(i1);
		*m.(bool_er).f_bool(Capture{}) = Bool(b1);
		return &m;
	};
	Method_MiniPerl6__Grammar.f_word = func(v_grammar *MiniPerl6__Grammar, v Capture) *Any {
		var s1 = tostr(v.p[0]);
		var i1 = toint(v.p[1]);
        var b1 = false;
        if i1 < len(s1) { 
		    var ch = int(s1[i1]);
		    b1 = unicode.IsLetter(ch);
		    if b1 {
			    i1++
            }
		}
		var m Any = new(MiniPerl6__Match);
		*m.(str_er).f_str(Capture{}) = *v.p[0];
		*m.(from_er).f_from(Capture{}) = *v.p[1];
		*m.(to_er).f_to(Capture{}) = Int(i1);
		*m.(bool_er).f_bool(Capture{}) = Bool(b1);
		return &m;
	};
	Method_MiniPerl6__Grammar.f_digit = func(v_grammar *MiniPerl6__Grammar, v Capture) *Any {
		var s1 = tostr(v.p[0]);
		var i1 = toint(v.p[1]);
        var b1 = false;
        if i1 < len(s1) { 
		    var ch = int(s1[i1]);
		    b1 = unicode.IsDigit(ch);
		    if b1 {
			    i1++
            }
		}
		var m Any = new(MiniPerl6__Match);
		*m.(str_er).f_str(Capture{}) = *v.p[0];
		*m.(from_er).f_from(Capture{}) = *v.p[1];
		*m.(to_er).f_to(Capture{}) = Int(i1);
		*m.(bool_er).f_bool(Capture{}) = Bool(b1);
		return &m;
	};
	Method_MiniPerl6__Grammar.f_not_newline = func(v_grammar *MiniPerl6__Grammar, v Capture) *Any {
		var s1 = tostr(v.p[0]);
		var i1 = toint(v.p[1]);
		var b1 = (s1[i1] != 13 && s1[i1] != 10);
		if b1 {
			i1++
		}
		var m Any = new(MiniPerl6__Match);
		*m.(str_er).f_str(Capture{}) = *v.p[0];
		*m.(from_er).f_from(Capture{}) = *v.p[1];
		*m.(to_er).f_to(Capture{}) = Int(i1);
		*m.(bool_er).f_bool(Capture{}) = Bool(b1);
		return &m;
	};
	Method_MiniPerl6__Grammar.f_space = func(v_grammar *MiniPerl6__Grammar, v Capture) *Any {
		var s1 = tostr(v.p[0]);
		var i1 = toint(v.p[1]);
        var b1 = false;
        if i1 < len(s1) { 
		    var ch = int(s1[i1]);
		    b1 = unicode.IsSpace(ch);
		    if b1 {
			    i1++
            }
		}
		var m Any = new(MiniPerl6__Match);
		*m.(str_er).f_str(Capture{}) = *v.p[0];
		*m.(from_er).f_from(Capture{}) = *v.p[1];
		*m.(to_er).f_to(Capture{}) = Int(i1);
		*m.(bool_er).f_bool(Capture{}) = Bool(b1);
		return &m;
	};

	Namespace_Main.f_perl_escape_string = func(v Capture) *Any {
		var s string = tostr(v.p[0]);
		var s1 string = "";
		for i := 0; i < len(s); i++ {
			switch {
			case s[i] == '\\':
				s1 += "\\\\"
			case s[i] == '\'':
				s1 += "\\'"
			default:
				s1 += s[i : i+1]
			}
		}
		return toStr(s1);
	};
	Namespace_Main.f_lisp_escape_string = func(v Capture) *Any {
		var s string = tostr(v.p[0]);
		var s1 string = "";
		for i := 0; i < len(s); i++ {
			switch {
			case s[i] == '\\':
				s1 += "\\\\"
			case s[i] == '"':
				s1 += "\\\""
			default:
				s1 += s[i : i+1]
			}
		}
		return toStr(s1);
	};

}
// end: Init_Prelude()

// end of the runtime lib
