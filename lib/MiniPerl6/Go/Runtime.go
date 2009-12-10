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

// interfaces used by the runtime

type Any interface {
    Int()   Int;
    Bool()  Bool;
    Str()   Str;
    Array() Array;
    Hash()  Hash;
    Equal(Any) Bool;
    Fetch() Any;
};

type Get_celler interface { 
    Get_cell () *Cell;
}
type Fetcher interface { 
    Fetch () Any;
}
type bind_er interface {
    Bind (Any) Any;
}
type join_er interface {
    f_join (v Capture) Any;
}
type perl_er interface {
    f_perl (v Capture) Any;
}
type scalar_er interface {
    f_scalar (v Capture) Any;
}
type isa_er interface {
    f_isa (v Capture) Any;
}

// constants

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

type Undef bool;
func (i Undef) Bool () Bool { return b_false }
func (i Undef) Int () Int { return i_0 }
func (i Undef) Str () Str { return s_empty }
func (i Undef) Array () Array { return a_array() }
func (i Undef) Hash () Hash { return h_hash() }
func (i Undef) Equal (j Any) Bool { return i.Str().Str_equal(j) }
func (i Undef) Fetch () Any { return i }
func (i Undef) f_perl (v1 Capture) Any {
    return Str{ s: "undef" }; 
}

type Bool struct {
    b bool;
}
func (i Bool) Bool () Bool { return i }
func (i Bool) Int () Int { if i.b { return i_1 }; return i_0 }
func (i Bool) Str () Str { if i.b { return s_true }; return s_false }
func (i Bool) Array () Array { panic("converting bool to array") }
func (i Bool) Hash () Hash { panic("converting bool to hash") }
func (i Bool) Equal (j Any) Bool { if i.b == j.Bool().b { return b_true }; return b_false }
func (i Bool) Fetch () Any { return i }
func (i Bool) Not () Bool { if i.b { return b_false }; return b_true }
func (i Bool) f_perl (v1 Capture) Any {
    return i.Str(); 
}

type Int struct {
    i int;
}
func (i Int) Bool () Bool { if i.i == 0 { return b_false }; return b_true }
func (i Int) Int () Int { return i }
func (i Int) Str () Str { return Str{ s: strconv.Itoa(i.i) } }
func (i Int) Array () Array { panic("converting int to array") }
func (i Int) Hash () Hash { panic("converting int to hash") }
func (i Int) Equal (j Any) Bool { if i.i == j.Int().i { return b_true }; return b_false }
func (i Int) Fetch () Any { return i }
func (i Int) f_perl (v1 Capture) Any {
    return i.Str(); 
}

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
func (i Str) Fetch () Any { return i }
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
func (i Str) f_perl (v1 Capture) Any {
    return Str{ s: 
        strings.Join( []string{ 
            "\"",
            i.s,
            "\"" }, "" ) };
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
func (i Function) Fetch () Any { return i }
func (f Function) Apply (p Capture) Any { return f.f(p) }
func (i Function) f_perl (v1 Capture) Any {
    return Str{ s: "sub { ... }" }; 
}

// type Method struct {
//     f func (Capture) Any;
// }
// func (f Method) Bool () Bool { return b_true }
// func (f Method) Int () Int { panic("converting function to int") }
// func (f Method) Str () Str { panic("converting function to string") }
// func (f Method) Array () Array { panic("converting function to array") }
// func (f Method) Hash () Hash { panic("converting function to hash") }
// func (f Method) Equal (j Any) Bool { panic("comparing function") }
// func (i Method) Fetch () Any { return i }
// func (f Method) Apply (p Capture) Any { return f.f(p) }
// func (i Method) f_perl (v1 Capture) Any {
//     return Str{ s: "method { ... }" }; 
// }

type Cell struct {
    c *Any;
}
type Scalar struct {
    s *Cell;
}
func (i Scalar) Bool () Bool { return i.s.c.Bool() }
func (i Scalar) Int () Int { return i.s.c.Int() }
func (i Scalar) Str () Str { return i.s.c.Str() }
func (i Scalar) Array () Array { return i.s.c.Array() }
func (i Scalar) Push (j Any) *Array {
    v := (*i.s.c).(Array);
    return v.Push(j) 
}
func (i Scalar) Hash () Hash { return i.s.c.Hash() }
func (i Scalar) Equal (j Any) Bool { return i.s.c.Equal(j) }
func (i Scalar) Apply (p Capture) Any { return (*i.s.c).(Function).Apply(p) }
func (i Scalar) Fetch () Any { 
    if i.s == nil {
        i.Store(u_undef);
    }
    return *i.s.c 
}
func (i Scalar) Get_cell () *Cell { return i.s }
func (i *Scalar) Store (j Any) Any { 
    if i.s == nil { i.s = new(Cell) };
    if i1, ok := j.(Fetcher); ok {
        v := i1.Fetch();
        i.s.c = &v;
    }
    else {
        i.s.c = &j; 
    }
    return j 
}
func (i *Scalar) Bind (j Any) Any { 
    if i1, ok := j.(Get_celler); ok {
        i.s = i1.Get_cell();
    }
    else {
        if i.s == nil { i.s = new(Cell) };
        i.s.c = &j; 
    }
    return j 
}
func (i Scalar) Defined() Any {
    if i.s == nil {
        return b_false;
    }
    switch i := (*i.s.c).(type) {
        case nil:       return b_false;
        case Undef:     return b_false;
    }
    return b_true;
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
func (i Hash) Fetch () Any { return i }
func (i Hash) Lookup (j Any) *Scalar {
    pos := j.Str().s;
    // TODO laziness
    if i.h == nil {
        i.h = make(map[string]*Scalar);
    }
    item, found := i.h[pos];
    if !found {
        item = new(Scalar);
        item.Store( u_undef );
        i.h[pos] = item;
    }
    return item;
}
func (i Hash) f_perl (v1 Capture) Any {
    return Str{ s: 
        strings.Join( []string{ 
            "{",
            "...",
            "}" }, " " ) };
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
    if i.n > 0 {
        s1 = v[0].Str().s 
    }
    for pos := 1; pos < i.n; pos++ {
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
func (i Array) Fetch () Any { return i }
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
    // (*i).v[(*i).n].Store( j );
    (*i).v[(*i).n].Bind( j );
    (*i).n++;
    return i
}
func (i *Array) f_pop () Any {
    (*i).n--;
    return (*i).v[(*i).n];
}
func (i Array) f_join (v1 Capture) Any {
    var s1 string;
    var sep string;
    if len(v1.p) > 0 {
        sep = v1.p[0].Str().s;
    }
    else {
        sep = "";
    }
    v := i.v;
    if i.n > 0 {
        s1 = v[0].Str().s
    }
    for pos := 1; pos < i.n; pos++ {
        s1 = strings.Join( []string{ s1, v[pos].Str().s }, sep );
    }
    return Str{s:s1};
}
func (i Array) f_perl (v1 Capture) Any {
    return Str{ s: 
        strings.Join( []string{ 
            "[",
            i.f_join( Capture{ p : []Any{ Str{s:", "} } } ).Str().s,
            "]" }, " " ) };
}

// Capture is a parameter list, for internal use 

type Capture struct {
    p []Any;
    // invocant *Any;
}

// runtime functions

func f_isa(s Capture) Any {
    var o = s.p[0];
    var name = s.p[1].Str().s;
    name = name;  // TODO
    if sc, ok := o.(Fetcher); ok {
        o = sc.Fetch()
    }
    if sc, ok := o.(isa_er); ok {
        return sc.f_isa(Capture{p: []Any{ s.p[1]} } )
    }
    return b_false;
}
func f_scalar(s Capture) Any {
    var o = s.p[0];
    if sc, ok := o.(Fetcher); ok {
        o = sc.Fetch()
    }
    if sc, ok := o.(scalar_er); ok {
        return sc.f_scalar(Capture{})
    }
    return o;
}
func Pop(s Any) Any {
    var o = s.Array();
    return o.f_pop()
}
func Die(s Capture) Any {
    var o = s.p[0];
    panic( o.Str().s );
}
func Print (s Capture) Any {
    for i, _ := range s.p {
        fmt.Print( s.p[i].Str().s );
    }
    return b_true;
}
func Print_stderr (s Capture) Any {
    for i, _ := range s.p {
        fmt.Fprint( os.Stderr, s.p[i].Str().s );
    }
    return b_true;
}
func Substr (s Capture) Str { 
    var a = s.p[1].Int().i;
    var b = s.p[2].Int().i;
    // TODO if b < 0
    return Str{ s : s.p[0].Str().s[ a : a + b ] } 
}
func Go_return (p chan Any, r Any) bool {
    p <- r;
    runtime.Goexit();
    return false; 
}

// end of the runtime lib

