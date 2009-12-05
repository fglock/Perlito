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
)

var i_1 = Int{i:1};
var i_0 = Int{i:0};
var b_true = Bool{b:true};
var b_false = Bool{b:false};
var s_true = Str{s:"true"};
var s_false = Str{s:"false"};
var s_empty = Str{s:""};
var u_undef = new(Undef);
func a_array () Array {
    return Array{ n : 0, v : make([]Scalar, 5) }
}
func h_hash () Hash {
    return Hash{ h : make(map[string]*Scalar) }
}

type Any interface {
    Int()   Int;
    Bool()  Bool;
    Str()   Str;
    Array() Array;
    Hash()  Hash;
    Equal(Any) Bool;
};

type Undef bool;
func (i Undef) Bool () Bool { return b_false }
func (i Undef) Int () Int { return i_0 }
func (i Undef) Str () Str { return s_empty }
func (i Undef) Array () Array { return a_array() }
func (i Undef) Hash () Hash { return h_hash() }
func (i Undef) Equal (j Any) Bool { return i.Str().Str_equal(j) }

type Bool struct {
    b bool;
}
func (i Bool) Bool () Bool { return i }
func (i Bool) Int () Int { if i.b { return i_1 }; return i_0 }
func (i Bool) Str () Str { if i.b { return s_true }; return s_false }
func (i Bool) Array () Array { panic("converting bool to array") }
func (i Bool) Hash () Hash { panic("converting bool to hash") }
func (i Bool) Equal (j Any) Bool { if i.b == j.Bool().b { return b_true }; return b_false }
func (i Bool) Not () Bool { if i.b { return b_false }; return b_true }

type Int struct {
    i int;
}
func (i Int) Bool () Bool { if i.i == 0 { return b_false }; return b_true }
func (i Int) Int () Int { return i }
func (i Int) Str () Str { return Str{ s: strconv.Itoa(i.i) } }
func (i Int) Array () Array { panic("converting int to array") }
func (i Int) Hash () Hash { panic("converting int to hash") }
func (i Int) Equal (j Any) Bool { if i.i == j.Int().i { return b_true }; return b_false }

type Str struct {
    s string;
}
func (i Str) Bool () Bool { 
    if i.s == "" || i.s == "0" { return b_false }; 
    return b_true 
}
func (i Str) Int () Int { 
    n, _ := strconv.Atoi(i.s); 
    return Int{i:n};
}
func (i Str) Str () Str { return i }
func (i Str) Array () Array { panic("converting string to array") }
func (i Str) Hash () Hash { panic("converting str to hash") }
func (i Str) Equal (j Any) Bool { if i.Int().i == j.Int().i { return b_true }; return b_false }
func (i Str) Str_equal(j Any) Bool {
    s1 := i.s;
    s2 := j.Str().s;
    if len(s1) != len(s2) {
        return b_false
    }
    for i := 0; i < len(s1); i++ {
        if s1[i] != s2[i] {
            return b_false
        }
    }
    return b_true;
}

type Function struct {
    f func (Capture) Any;
}
func (f Function) Bool () Bool { return b_true }
func (f Function) Int () Int { panic("converting function to int") }
func (f Function) Str () Str { panic("converting function to string") }
func (f Function) Array () Array { panic("converting function to array") }
func (f Function) Hash () Hash { panic("converting function to hash") }
func (f Function) Equal (j Any) Bool { panic("comparing function") }
func (f Function) Apply (p Capture) Any { return f.f(p) }

type Method struct {
    f func (Capture) Any;
}
func (f Method) Bool () Bool { return b_true }
func (f Method) Int () Int { panic("converting function to int") }
func (f Method) Str () Str { panic("converting function to string") }
func (f Method) Array () Array { panic("converting function to array") }
func (f Method) Hash () Hash { panic("converting function to hash") }
func (f Method) Equal (j Any) Bool { panic("comparing function") }
func (f Method) Apply (p Capture) Any { return f.f(p) }

type Get_celler interface { 
    Get_cell () *Any;
}
type Fetcher interface { 
    Fetch () Any;
}

type Scalar struct {
    s *Any;
}
func (i Scalar) Bool () Bool { return i.s.Bool() }
func (i Scalar) Int () Int { return i.s.Int() }
func (i Scalar) Str () Str { return i.s.Str() }
func (i Scalar) Array () Array { return i.s.Array() }
func (i Scalar) Push (j Any) *Array {
    v := (*i.s).(Array);
    return v.Push(j) 
}
func (i Scalar) Hash () Hash { return i.s.Hash() }
func (i Scalar) Equal (j Any) Bool { return i.s.Equal(j) }
func (i Scalar) Apply (p Capture) Any { return (*i.s).(Function).Apply(p) }
func (i Scalar) Fetch () Any { return *i.s }
func (i Scalar) Get_cell () *Any { return i.s }
func (i *Scalar) Store (j Any) Any { 
    if i1, ok := j.(Fetcher); ok {
        v := i1.Fetch();
        i.s = &v;
    }
    else {
        i.s = &j; 
    }
    return j 
}
func (i *Scalar) Bind (j Any) Any { 
    if i1, ok := j.(Get_celler); ok {
        i.s = i1.Get_cell();
    }
    else {
        i.s = &j; 
    }
    return j 
}

type Hash struct {
    h map[string]*Scalar;
}
func (i Hash) Bool () Bool { 
    if len(i.h) == 0 { return b_false };
    return b_true;
}
func (i Hash) Int () Int { 
    return Int{i: len(i.h) } 
}
func (i Hash) Str () Str { 
    return Str{s: "TODO: hash.Str" };
}
func (i Hash) Array () Array { 
    return a_array(); // TODO 
}
func (i Hash) Hash () Hash { 
    return i 
}
func (i Hash) Equal (j Any) Bool { 
    return b_false;  // TODO 
}
func (i Hash) Lookup (j Any) *Scalar {
    pos := j.Str().s;
    // TODO laziness
    item, found := i.h[pos];
    if !found {
        item = new(Scalar);
        item.Store( u_undef );
        i.h[pos] = item;
    }
    return item;
}

type Array struct {
    n int;
    v []Scalar;
}
func (i Array) Bool () Bool { 
    if i.n == 0 { return b_false };
    return b_true;
}
func (i Array) Int () Int { 
    return Int{i: i.n + 1 } 
}
func (i Array) Str () Str { 
    var s1 string;
    v := i.v;
    for pos := 0; pos <= i.n; pos++ {
        s1 = strings.Join( []string{ s1, v[pos].Str().s }, " " );
    }
    return Str{s:s1};
}
func (i Array) Array () Array { 
    return i 
}
func (i Array) Hash () Hash { 
    return h_hash(); // TODO 
}
func (i Array) Equal (j Any) Bool { 
    return b_false;  // TODO 
}
func (i Array) Index (j Any) *Scalar {
    pos := j.Int().i;
    // TODO autoextend
    // TODO laziness
    if i.v[pos].s == nil {
        i.v[pos].Store( u_undef );  // is this needed?
    }
    return &i.v[pos];
}
func (i *Array) Push (j Any) *Array {
    (*i).v[(*i).n].Store( j );
    (*i).n++;
    return i
}

// Capture is a parameter list, for internal use 

type Capture struct {
    p []Any;
    // invocant *Any;
}

// runtime functions

func Print (s Capture) {
    for i, _ := range s.p {
        fmt.Print( s.p[i].Str().s );
    }
}
func Print_stderr (s Capture) {
    for i, _ := range s.p {
        fmt.Fprint( os.Stderr, s.p[i].Str().s );
    }
}
func Substr (s Capture) Str { 
    var a = s.p[0].Int().i;
    var b = s.p[1].Int().i;
    // TODO if b < 0
    return Str{ s : s.p[0].Str().s[ a : a + b ] } 
}

// 'return' exception

func Return (p chan Any, r Any) bool {
    p <- r;
    runtime.Goexit();
    return false; 
}

// end of the runtime lib

