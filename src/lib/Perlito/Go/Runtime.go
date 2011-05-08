// lib/Perlito/Go/Runtime.go
//
// Runtime for "Perlito" Perlito-in-Go
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
// Copyright 2009, 2011 by Flavio Soibelmann Glock and others.
//
// This program is free software; you can redistribute it and/or modify it
// under the same terms as Perl itself.
//
// See http://www.perl.com/perl/misc/Artistic.html

package main

import (
	"fmt"
	"strconv"
	"os"
	"strings"
	"runtime"
	"unicode"
	// "reflect";
	"io/ioutil"
	"utf8"
)

// interfaces used by the runtime

type Any interface{}

type join_er interface {
	f_join(v Capture) *Any
}
type perl_er interface {
	f_perl(v Capture) *Any
}
type scalar_er interface {
	f_scalar(v Capture) *Any
}
type isa_er interface {
	f_isa(v Capture) *Any
}
type values_er interface {
	f_values(v Capture) *Any
}
type keys_er interface {
	f_keys(v Capture) *Any
}
type int_er interface {
	f_int(v Capture) *Any
}
type num_er interface {
	f_num(v Capture) *Any
}
type str_er interface {
	f_str(v Capture) *Any
}
type Str_er interface {
	f_Str(v Capture) *Any
}
type bool_er interface {
	f_bool(v Capture) *Any
}
type Bool_er interface {
	f_Bool(v Capture) *Any
}
type array_er interface {
	f_array(v Capture) *Any
}
type hash_er interface {
	f_hash(v Capture) *Any
}
type push_er interface {
	f_push(v Capture) *Any
}
type pop_er interface {
	f_pop(v Capture) *Any
}
type shift_er interface {
	f_shift(v Capture) *Any
}
type lookup_er interface {
	f_lookup(v Capture) *Any
}
type index_er interface {
	f_index(v Capture) *Any
}
type function_er interface {
	f_function(v Capture) *Any
}
type exists_er interface {
	f_exists(v Capture) *Any
}


// constants
var i_1 = toInt(1)
var i_0 = toInt(0)
var s_true = toStr("True")
var s_false = toStr("False")
var s_empty = toStr("")
var s_undef = toStr("undef")

func b_true() *Any {
	var v Any = Bool(true)
	return &v
}
func b_false() *Any {
	var v Any = Bool(false)
	return &v
}
func u_undef() *Any {
	var v Any = new(Undef)
	return &v
}
func a_array() *Any {
	var a = new(Array)
	a.n = -1
	a.v = make([]*Any, 5)
	var v Any = a
	return &v
}
func h_hash() *Any {
	var h = new(Hash)
	h.h = make(map[string]*Any)
	var v Any = h
	return &v
}

type Undef bool

func (i Undef) f_Bool(Capture) *Any  { return b_false() }
func (i Undef) f_int(Capture) *Any   { return i_0 }
func (i Undef) f_num(Capture) *Any   { return toNum(0.0) }
func (i Undef) f_Str(Capture) *Any   { return s_empty }
func (i Undef) f_array(Capture) *Any { return a_array() }
func (i Undef) f_hash(Capture) *Any  { return h_hash() }
func (i Undef) f_perl(Capture) *Any  { return s_undef }
func (i Undef) f_isa(v Capture) *Any { return toBool("Undef" == tostr(v.p[0])) }

type Bool bool

func toBool(i bool) *Any {
	var r Any = Bool(i)
	return &r
}
func toBit(i int) *Any {
	var r Any = Bool(i != 0)
	return &r
}
func tobool(v *Any) bool { return bool((*((*v).(Bool_er).f_Bool(Capture{}))).(Bool)) }
func (i Bool) f_Bool(Capture) *Any {
	var v Any = i
	return &v
}
func (i Bool) f_int(Capture) *Any {
	if i {
		return i_1
	}
	return i_0
}
func (i Bool) f_Str(Capture) *Any {
	if i {
		return s_true
	}
	return s_false
}
func (i Bool) f_array(Capture) *Any { panic("converting bool to array") }
func (i Bool) f_hash(Capture) *Any  { panic("converting bool to hash") }
func (i Bool) f_not(Capture) *Any {
	if i {
		return b_false()
	}
	return b_true()
}
func (i Bool) f_perl(Capture) *Any  { return i.f_Str(Capture{}) }
func (i Bool) f_isa(v Capture) *Any { return toBool("Bool" == tostr(v.p[0])) }

type Int int

func toInt(i int) *Any {
	var r Any = Int(i)
	return &r
}
func toint(v *Any) int { return int((*((*v).(int_er).f_int(Capture{}))).(Int)) }
func (i Int) f_Bool(Capture) *Any {
	if i == 0 {
		return b_false()
	}
	return b_true()
}
func (i Int) f_num(Capture) *Any {
	var v Any = i // TODO
	return &v
}
func (i Int) f_int(Capture) *Any {
	var v Any = i
	return &v
}
func (i Int) f_Str(Capture) *Any   { return toStr(strconv.Itoa(int(i))) }
func (i Int) f_array(Capture) *Any { panic("converting int to array") }
func (i Int) f_hash(Capture) *Any  { panic("converting int to hash") }
func (i Int) f_perl(Capture) *Any  { return i.f_Str(Capture{}) }
func (i Int) f_isa(v Capture) *Any { return toBool("Int" == tostr(v.p[0])) }

type Num float

func toNum(i float) *Any {
	var r Any = Num(i)
	return &r
}
func tonum(v *Any) float { return float((*((*v).(num_er).f_num(Capture{}))).(Num)) }
func (i Num) f_Bool(Capture) *Any {
	if i == 0 {
		return b_false()
	}
	return b_true()
}
func (i Num) f_num(Capture) *Any {
	var v Any = i
	return &v
}
func (i Num) f_int(Capture) *Any {
	var v Any = Int(int(i))
	return &v
}
func (i Num) f_Str(Capture) *Any   { return toStr(strconv.Ftoa(float(i), 'g', -1)) }
func (i Num) f_array(Capture) *Any { panic("converting number to array") }
func (i Num) f_hash(Capture) *Any  { panic("converting number to hash") }
func (i Num) f_perl(Capture) *Any  { return i.f_Str(Capture{}) }
func (i Num) f_isa(v Capture) *Any { return toBool("Num" == tostr(v.p[0])) }

type Str string

func toStr(i string) *Any {
	var r Any = Str(i)
	return &r
}
// func tostr(v *Any) string	{ return string((*((*v).(Str_er).f_Str(Capture{}))).(Str)) }
func tostr(v *Any) string {
	s, ok := (*v).(Str_er)
	if ok {
		return string((*s.f_Str(Capture{})).(Str))
	}
	return "<Object can't .Str>"
}
func (i Str) f_Bool(Capture) *Any {
	if i == "" || i == "0" {
		return b_false()
	}
	return b_true()
}
func (i Str) f_int(Capture) *Any {
	n, _ := strconv.Atoi(string(i))
	return toInt(n)
}
func (i Str) f_Str(Capture) *Any {
	var j Any = i
	return &j
}
func (i Str) f_array(Capture) *Any { panic("converting string to array") }
func (i Str) f_hash(Capture) *Any  { panic("converting str to hash") }
func (i Str) f_str_equal(v Capture) *Any {
	s2 := tostr(v.p[0])
	if string(i) != s2 {
		return b_false()
	}
	return b_true()
}
func (i Str) f_perl(Capture) *Any {
	var s Any = i
	return toStr("'" + tostr(Namespace_Main.f_perl_escape_string(Capture{p: []*Any{&s}})) + "'")
}
func (i Str) f_isa(v Capture) *Any   { return toBool("Str" == tostr(v.p[0])) }
func (i Str) f_chars(v Capture) *Any { return toInt(len(string(i))) }


type Function func(Capture) *Any

func (f Function) f_function(v Capture) *Any { return f(v) }
func toFunction(f func(Capture) *Any) *Any {
	var r Any = Function(f)
	return &r
}

type Hash struct {
	h map[string]*Any
}

func (i Hash) f_Bool(Capture) *Any {
	if len(i.h) == 0 {
		return b_false()
	}
	return b_true()
}
func (i Hash) f_int(Capture) *Any { return toInt(len(i.h)) }
func (i Hash) f_Str(Capture) *Any { return toStr("TODO: hash.Str") }
func (i Hash) f_array(Capture) *Any {
	return a_array() // TODO
}
func (i *Hash) f_hash(Capture) *Any {
	var p Any
	var j *Any = &p
	*j = i
	return j
}
func (i *Hash) f_lookup(v Capture) *Any {
	pos := tostr(v.p[0])
	// TODO laziness
	if i.h == nil {
		i.h = make(map[string]*Any)
	}
	item, found := i.h[pos]
	if !found {
		var j *Any = u_undef()
		i.h[pos] = j
		return j
	}
	return item
}
func (i *Hash) f_exists(v Capture) *Any {
	if i.h == nil {
		return b_false()
	}
	pos := tostr(v.p[0])
	_, found := i.h[pos]
	if found {
		return b_true()
	}
	return b_false()
}
func (i Hash) f_perl(Capture) *Any {
	var s = "{"
	var sep = ""
	for key, value := range i.h {
		s = s + sep + key + " => " + tostr((*value).(perl_er).f_perl(Capture{}))
		sep = ", "
	}
	s = s + "}"
	return toStr(s)
}
func (i Hash) f_isa(v Capture) *Any { return toBool("Hash" == tostr(v.p[0])) }
func (i Hash) f_values(v1 Capture) *Any {
	a := a_array()
	for _, value := range i.h {
		(*a).(push_er).f_push(Capture{p: []*Any{value}})
	}
	return a
}
func (i *Hash) f_keys(v1 Capture) *Any {
	a := a_array()
	for key, _ := range i.h {
		(*a).(push_er).f_push(Capture{p: []*Any{toStr(key)}})
	}
	return a
}

type Array struct {
	n int
	v []*Any
}

func (i Array) f_Bool(Capture) *Any {
	if i.n < 0 {
		return b_false()
	}
	return b_true()
}
func (i Array) f_int(Capture) *Any { return toInt(i.n + 1) }
func (i Array) f_Str(Capture) *Any { return i.f_join(Capture{p: []*Any{toStr(" ")}}) }
func (i *Array) f_array(Capture) *Any {
	var p Any
	var j *Any = &p
	*j = i
	return j
}
func (i Array) f_hash(Capture) *Any {
	return h_hash() // TODO
}
func (i *Array) f_index(v Capture) *Any {
	pos := toint(v.p[0])
	// TODO autoextend
	// TODO laziness
	if i.v[pos] == nil {
		var j *Any = u_undef()
		i.v[pos] = j
		return j
	}
	return i.v[pos]
}
func (i *Array) f_push(v Capture) *Any {
	i.n++
	if i.n >= len(i.v) {
		v2 := make([]*Any, len(i.v)+len(i.v))
		for i, x := range i.v {
			v2[i] = x
		}
		i.v = v2
	}
	if i.v[i.n] == nil {
		var v Any
		i.v[i.n] = &v
	}
	*i.v[i.n] = *v.p[0]
	var v2 Any = i
	return &v2
}
func (i *Array) f_pop(Capture) *Any {
	i.n--
	return i.v[i.n+1]
}
func (i *Array) f_shift(v Capture) *Any {
	r := i.v[0]
	i.n--
	for pos := 0; pos <= i.n; pos++ {
		i.v[pos] = i.v[pos+1]
	}
	return r
}
func (i Array) f_join(v1 Capture) *Any {
	var s1 string
	var sep string
	if len(v1.p) > 0 {
		sep = tostr(v1.p[0])
	} else {
		sep = ""
	}
	v := i.v
	if i.n >= 0 {
		s1 = tostr(v[0])
	}
	for pos := 1; pos <= i.n; pos++ {
		s1 = s1 + sep + tostr(v[pos])
	}
	return toStr(s1)
}
func (i Array) f_perl(v1 Capture) *Any {
	var s = "["
	var sep = ""
	for pos := 0; pos <= i.n; pos++ {
		s = s + sep + tostr((*i.v[pos]).(perl_er).f_perl(Capture{}))
		sep = ", "
	}
	s = s + "]"
	return toStr(s)
}
func (i Array) f_isa(v Capture) *Any { return toBool("Array" == tostr(v.p[0])) }

// Capture is a parameter list, for internal use

type Capture struct {
	p []*Any
	// invocant *Any;
}

// runtime functions

func f_isa(s Capture) *Any {
	if sc, ok := (*s.p[0]).(isa_er); ok {
		return sc.f_isa(Capture{p: []*Any{s.p[1]}})
	}
	var r Any = b_false
	return &r
}
func f_scalar(s Capture) *Any {
	if sc, ok := (*s.p[0]).(scalar_er); ok {
		return sc.f_scalar(Capture{})
	}
	return s.p[0]
}
func f_pop(s Capture) *Any {
	return (*(*s.p[0]).(array_er).f_array(Capture{})).(pop_er).f_pop(Capture{})
}
func f_push(s Capture) *Any {
	var o = (*(*s.p[0]).(array_er).f_array(Capture{})).(push_er).f_push(Capture{p: []*Any{s.p[1]}})
	return o
}
func f_shift(s Capture) *Any {
	return (*(*s.p[0]).(array_er).f_array(Capture{})).(shift_er).f_shift(Capture{})
}
func f_index(v Capture) *Any {
	var s = tostr(v.p[0])
	var sep = tostr(v.p[1])
	return toInt(strings.Index(s, sep))
}
func f_defined(v Capture) *Any {
	switch i := (*v.p[0]).(type) {
	case nil:
		return b_false()
	case Undef:
		return b_false()
	}
	return b_true()
}
func f_die(v Capture) *Any { panic(tostr(v.p[0])) }
func f_print(s Capture) *Any {
	for i, _ := range s.p {
		// var t = reflect.Typeof( *s.p[i] );
		// fmt.Println( "type: ", t );
		fmt.Print(tostr(s.p[i]))
	}
	return b_true()
}
func f_print_stderr(s Capture) *Any {
	for i, _ := range s.p {
		fmt.Fprint(os.Stderr, tostr(s.p[i]))
	}
	return b_true()
}
func f_substr(s Capture) *Any {
	var s1 = tostr(s.p[0])
	var a = toint(s.p[1])
	var b = toint(s.p[2])
	if a >= len(s1) {
		return toStr("")
	}
	var c = a + b
	// TODO if b < 0
	if c >= len(s1) {
		c = len(s1)
	}
	return toStr(s1[a:c])
}
func f_and(f1, f2 func() *Any) *Any {
	var tmp = f1()
	if tobool(tmp) {
		return f2()
	}
	return tmp
}
func f_or(f1, f2 func() *Any) *Any {
	var tmp = f1()
	if tobool(tmp) {
		return tmp
	}
	return f2()
}
func f_numify(v *Any) *Any {
	switch i := (*v).(type) {
	case nil:
		return i_0
	case Undef:
		return i_0
	case Int:
		return v
	case Num:
		return v
	case Str:
		// Str can be converted to Int or Num
		s := string((*v).(Str))
		out := ""
		pos := 0
		max := len(s)
		if pos >= max {
			return i_0
		}
		if s[pos] == '+' {
			pos++
		} else if s[pos] == '-' {
			out += s[pos : pos+1]
			pos++
		}
		if pos >= max || s[pos] < '0' || s[pos] > '9' {
			return i_0
		}
		for i := pos; i < len(s); i++ {
			if s[i] >= '0' && s[i] <= '9' {
				out += s[i : i+1]
				pos++
			} else {
				i = len(s)
			}
		}
		if (pos < max && s[pos] == '.') || ((pos+1) < max && (s[pos] == 'e' || s[pos] == 'E') && (s[pos+1] == '+' || s[pos+1] == '-' || (s[pos+1] >= '0' && s[pos+1] <= '9'))) {
			// 123. 123e10
			n, _ := strconv.Atof(s)
			return toNum(n)
		}
		n, _ := strconv.Atoi(out)
		return toInt(n)
	}
	return (*v).(int_er).f_int(Capture{})
}
func f_add(v1, v2 *Any) *Any {
	n1 := f_numify(v1);
    n2 := f_numify(v2);
	switch i := (*n1).(type) {
	case Int:
	    switch j := (*n2).(type) {
	    case Int:
            return toInt( int((*n1).(Int)) + int((*n2).(Int)) )
        case Num:
	    	return toNum( float((*n1).(Int)) + float((*n2).(Num)) )
	    }
    case Num:
	    switch j := (*n2).(type) {
        case Int:
            return toNum( float((*n1).(Num)) + float((*n2).(Int)) )
        case Num:
            return toNum( float((*n1).(Num)) + float((*n2).(Num)) )
	    }
	}
    return i_0
}
func f_sub(v1, v2 *Any) *Any {
	n1 := f_numify(v1);
    n2 := f_numify(v2);
	switch i := (*n1).(type) {
	case Int:
	    switch j := (*n2).(type) {
	    case Int:
            return toInt( int((*n1).(Int)) - int((*n2).(Int)) )
        case Num:
	    	return toNum( float((*n1).(Int)) - float((*n2).(Num)) )
	    }
    case Num:
	    switch j := (*n2).(type) {
        case Int:
            return toNum( float((*n1).(Num)) - float((*n2).(Int)) )
        case Num:
            return toNum( float((*n1).(Num)) - float((*n2).(Num)) )
	    }
	}
    return i_0
}
func f_mul(v1, v2 *Any) *Any {
	n1 := f_numify(v1);
    n2 := f_numify(v2);
	switch i := (*n1).(type) {
	case Int:
	    switch j := (*n2).(type) {
	    case Int:
            return toInt( int((*n1).(Int)) * int((*n2).(Int)) )
        case Num:
	    	return toNum( float((*n1).(Int)) * float((*n2).(Num)) )
	    }
    case Num:
	    switch j := (*n2).(type) {
        case Int:
            return toNum( float((*n1).(Num)) * float((*n2).(Int)) )
        case Num:
            return toNum( float((*n1).(Num)) * float((*n2).(Num)) )
	    }
	}
    return i_0
}
func f_div(v1, v2 *Any) *Any {
	n1 := f_numify(v1);
    n2 := f_numify(v2);
	switch i := (*n1).(type) {
	case Int:
	    switch j := (*n2).(type) {
	    case Int:
            return toInt( int((*n1).(Int)) / int((*n2).(Int)) )
        case Num:
	    	return toNum( float((*n1).(Int)) / float((*n2).(Num)) )
	    }
    case Num:
	    switch j := (*n2).(type) {
        case Int:
            return toNum( float((*n1).(Num)) / float((*n2).(Int)) )
        case Num:
            return toNum( float((*n1).(Num)) / float((*n2).(Num)) )
	    }
	}
    return i_0
}
func f_greater(v1, v2 *Any) *Any {
	n1 := f_numify(v1);
    n2 := f_numify(v2);
	switch i := (*n1).(type) {
	case Int:
	    switch j := (*n2).(type) {
	    case Int:
            return toBool( int((*n1).(Int)) > int((*n2).(Int)) )
        case Num:
	    	return toBool( float((*n1).(Int)) > float((*n2).(Num)) )
	    }
    case Num:
	    switch j := (*n2).(type) {
        case Int:
            return toBool( float((*n1).(Num)) > float((*n2).(Int)) )
        case Num:
            return toBool( float((*n1).(Num)) > float((*n2).(Num)) )
	    }
	}
    return b_false();
}
func f_smaller(v1, v2 *Any) *Any {
	n1 := f_numify(v1);
    n2 := f_numify(v2);
	switch i := (*n1).(type) {
	case Int:
	    switch j := (*n2).(type) {
	    case Int:
            return toBool( int((*n1).(Int)) < int((*n2).(Int)) )
        case Num:
	    	return toBool( float((*n1).(Int)) < float((*n2).(Num)) )
	    }
    case Num:
	    switch j := (*n2).(type) {
        case Int:
            return toBool( float((*n1).(Num)) < float((*n2).(Int)) )
        case Num:
            return toBool( float((*n1).(Num)) < float((*n2).(Num)) )
	    }
	}
    return b_false();
}
func f_greater_equal(v1, v2 *Any) *Any {
	n1 := f_numify(v1);
    n2 := f_numify(v2);
	switch i := (*n1).(type) {
	case Int:
	    switch j := (*n2).(type) {
	    case Int:
            return toBool( int((*n1).(Int)) >= int((*n2).(Int)) )
        case Num:
	    	return toBool( float((*n1).(Int)) >= float((*n2).(Num)) )
	    }
    case Num:
	    switch j := (*n2).(type) {
        case Int:
            return toBool( float((*n1).(Num)) >= float((*n2).(Int)) )
        case Num:
            return toBool( float((*n1).(Num)) >= float((*n2).(Num)) )
	    }
	}
    return b_false();
}
func f_smaller_equal(v1, v2 *Any) *Any {
	n1 := f_numify(v1);
    n2 := f_numify(v2);
	switch i := (*n1).(type) {
	case Int:
	    switch j := (*n2).(type) {
	    case Int:
            return toBool( int((*n1).(Int)) <= int((*n2).(Int)) )
        case Num:
	    	return toBool( float((*n1).(Int)) <= float((*n2).(Num)) )
	    }
    case Num:
	    switch j := (*n2).(type) {
        case Int:
            return toBool( float((*n1).(Num)) <= float((*n2).(Int)) )
        case Num:
            return toBool( float((*n1).(Num)) <= float((*n2).(Num)) )
	    }
	}
    return b_false();
}

// implementation of functions and methods declared in the prelude file

func (v_self Perlito__Match) f_int(Capture) *Any {
	return (*v_self.f_Str(Capture{})).(int_er).f_int(Capture{})
}

var List_ARGS *Any // @*ARGS

func Init_Prelude() {

	// initialize @*ARGS
	List_ARGS = a_array()
	for i, _ := range os.Args {
		if i > 0 {
			(*List_ARGS).(push_er).f_push(Capture{p: []*Any{toStr(os.Args[i])}})
		}
	}

	Method_Perlito__Match.f_scalar = func(v_self *Perlito__Match, v Capture) *Any {
		if v_self.v_bool == nil {
			v_self.v_bool = toBool(false)
		}
		if tobool(v_self.v_bool) {
			if v_self.v_capture == nil {
				var x Any
				v_self.v_capture = &x
			}
			if tobool(f_defined(Capture{p: []*Any{v_self.v_capture}})) {
				return v_self.v_capture
			}
			return f_substr(Capture{p: []*Any{v_self.v_str, v_self.v_from, toInt(toint(v_self.v_to) - toint(v_self.v_from))}})
		}
		return toStr("")
	}
	Method_Perlito__Match.f_Str = func(v_self *Perlito__Match, v Capture) *Any {
		if v_self.v_bool == nil {
			v_self.v_bool = toBool(false)
		}
		if tobool(v_self.v_bool) {
			if v_self.v_capture == nil {
				var x Any
				v_self.v_capture = &x
			}
			if tobool(f_defined(Capture{p: []*Any{v_self.v_capture}})) {
				return v_self.v_capture
			}
			return f_substr(Capture{p: []*Any{v_self.v_str, v_self.v_from, toInt(toint(v_self.v_to) - toint(v_self.v_from))}})
		}
		return toStr("")
	}
	Method_Perlito__Match.f_exists = func(v_self *Perlito__Match, v Capture) *Any {
        tmp := v_self.f_hash(Capture{})
        return (*tmp).(exists_er).f_exists(v)
	}

	Method_Perlito__Grammar.f_is_newline = func(v_grammar *Perlito__Grammar, v Capture) *Any {
		var s1 = tostr(v.p[0])
		var i1 = toint(v.p[1])
		var b1 = false
		if (i1+1) < len(s1) && ((s1[i1] == 13 && s1[i1+1] == 10) || (s1[i1] == 10 && s1[i1+1] == 13)) {
			i1 += 2
			b1 = true
		} else {
			if i1 < len(s1) && (s1[i1] == 13 || s1[i1] == 10) {
				i1 += 1
				b1 = true
			}
		}
		var m Any = new(Perlito__Match)
		*m.(str_er).f_str(Capture{}) = *v.p[0]
		*m.(from_er).f_from(Capture{}) = *v.p[1]
		*m.(to_er).f_to(Capture{}) = Int(i1)
		*m.(bool_er).f_bool(Capture{}) = Bool(b1)
		return &m
	}
	Method_Perlito__Grammar.f_word = func(v_grammar *Perlito__Grammar, v Capture) *Any {
		var s1 = tostr(v.p[0])
		var i1 = toint(v.p[1])
		var b1 = false
		if i1 < len(s1) {
			var ch = int(s1[i1])
			b1 = unicode.IsLetter(ch)
			if b1 {
				i1++
			}
		}
		var m Any = new(Perlito__Match)
		*m.(str_er).f_str(Capture{}) = *v.p[0]
		*m.(from_er).f_from(Capture{}) = *v.p[1]
		*m.(to_er).f_to(Capture{}) = Int(i1)
		*m.(bool_er).f_bool(Capture{}) = Bool(b1)
		return &m
	}
	Method_Perlito__Grammar.f_digit = func(v_grammar *Perlito__Grammar, v Capture) *Any {
		var s1 = tostr(v.p[0])
		var i1 = toint(v.p[1])
		var b1 = false
		if i1 < len(s1) {
			var ch = int(s1[i1])
			b1 = unicode.IsDigit(ch)
			if b1 {
				i1++
			}
		}
		var m Any = new(Perlito__Match)
		*m.(str_er).f_str(Capture{}) = *v.p[0]
		*m.(from_er).f_from(Capture{}) = *v.p[1]
		*m.(to_er).f_to(Capture{}) = Int(i1)
		*m.(bool_er).f_bool(Capture{}) = Bool(b1)
		return &m
	}
	Method_Perlito__Grammar.f_space = func(v_grammar *Perlito__Grammar, v Capture) *Any {
		var s1 = tostr(v.p[0])
		var i1 = toint(v.p[1])
		var b1 = false
		if i1 < len(s1) {
			var ch = int(s1[i1])
			b1 = unicode.IsSpace(ch)
			if b1 {
				i1++
			}
		}
		var m Any = new(Perlito__Match)
		*m.(str_er).f_str(Capture{}) = *v.p[0]
		*m.(from_er).f_from(Capture{}) = *v.p[1]
		*m.(to_er).f_to(Capture{}) = Int(i1)
		*m.(bool_er).f_bool(Capture{}) = Bool(b1)
		return &m
	}

	Namespace_Main.f_lisp_escape_string = func(v Capture) *Any {
		var s string = tostr(v.p[0])
		var s1 string = ""
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
		return toStr(s1)
	}

	Namespace_Main.f_to_lisp_namespace = func(v Capture) *Any {
		var s string = tostr(v.p[0])
		var s1 string = ""
		for i := 0; i < len(s); i++ {
			switch {
			case s[i] == ':':
				s1 += "-"
				i1 := i + 1
				if i1 < len(s) && s[i1] == ':' {
					i = i1
				}
			default:
				s1 += s[i : i+1]
			}
		}
		return toStr("mp-" + s1)
	}
	Namespace_Main.f_to_go_namespace = func(v Capture) *Any {
		var s string = tostr(v.p[0])
		var s1 string = ""
		for i := 0; i < len(s); i++ {
			switch {
			case s[i] == ':':
				s1 += "_"
			default:
				s1 += s[i : i+1]
			}
		}
		return toStr(s1)
	}
	Namespace_Main.f_to_javascript_namespace = func(v Capture) *Any {
		var s string = tostr(v.p[0])
		var s1 string = ""
		for i := 0; i < len(s); i++ {
			switch {
			case s[i] == ':':
				s1 += "$"
				i1 := i + 1
				if i1 < len(s) && s[i1] == ':' {
					i = i1
				}
			default:
				s1 += s[i : i+1]
			}
		}
		return toStr(s1)
	}
	Namespace_IO.f_slurp = func(cap Capture) *Any {
		var filename = tostr(cap.p[0])
		s, error := ioutil.ReadFile(filename)
		if error != nil {
			return u_undef()
		}
		b := make([]byte, len(s))
		w := 0
		for r := 0; r < len(s); {
			switch {
			// ASCII
			case s[r] < utf8.RuneSelf:
				b[w] = s[r]
				r++
				w++
			// Coerce to well-formed UTF-8.
			default:
				rune, size := utf8.DecodeRune(s[r : len(s)-1])
				r = r + size
				w = w + utf8.EncodeRune(rune, b[w:len(b)-1])
			}
		}
		return toStr(string(b[0:w]))
	}
}
// end: Init_Prelude()

// end of the runtime lib
