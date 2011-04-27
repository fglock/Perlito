"""
Perlito__Python__Runtime

DESCRIPTION

Provides runtime routines for the Perlito-in-Python compiled code

AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

COPYRIGHT

Copyright 2010, 2011 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

"""

import sys
import re
import __builtin__

recursion = sys.getrecursionlimit()
if recursion < 2000:
    sys.setrecursionlimit(2000)

__all__ = ['mp6_to_num', 'mp6_to_scalar', 'mp6_to_bool', 'mp6_isa',
           'mp6_and', 'mp6_or', 'mp6_defined_or',
           'mp6_join', 'mp6_index', 'mp6_id', 'mp6_split',
           'mp6_Mu', 
           'mp6_Array', 'mp6_Hash', 'mp6_Scalar',
           'mp6_Return', 
           'Perlito__Match',
           'Perlito__Grammar', 'Perlito__Grammar_proto', 
           'Main', 'Main_proto',
           'IO', 'IO_proto']

def f_print(*msg):
    for m in msg:
        sys.stdout.write(unicode(m).encode('utf-8'))
    return 1;
__builtin__.f_print = f_print

def f_say(*msg):
    for m in msg:
        sys.stdout.write(unicode(m).encode('utf-8'))
    sys.stdout.write("\n")
    return 1;
__builtin__.f_say = f_say

def f_warn(*msg):
    for m in msg:
        sys.stderr.write(unicode(m).encode('utf-8'))
    sys.stderr.write("\n")
__builtin__.f_warn = f_warn

def f_die(*msg):
    sys.stderr.write("Died: ")
    for m in msg:
        sys.stderr.write(unicode(m).encode('utf-8'))
    sys.stderr.write("\n")
    raise mp6_Die()
__builtin__.f_die = f_die

def f_map(v, f):
    try:
        return v.f_map(f)
    except AttributeError:
        return mp6_Array([]) 
__builtin__.f_map = f_map

def f_chr(n):
    return unichr(mp6_to_num(n))
__builtin__.f_chr = f_chr

def f_ord(n):
    return ord(unicode(n))
__builtin__.f_ord = f_ord

def mp6_to_scalar(v):
    try:
        return v.f_scalar()
    except AttributeError:
        return v

def mp6_to_bool(o):
    try: 
        return o.f_bool()
    except AttributeError:
        if type(o) == type(False):
            return o
        if type(o) == type(1):
            return o != 0
        if type(o) == type(1.1):
            return o != 0
        if type(o) == type("a"):
            return o != "" and o != "0"
        if type(o) == type(u"a"):
            return o != u"" and o != u"0"
        return False

def mp6_and(x, y):
    if mp6_to_bool(x):
        return y()
    return x

def mp6_or(x, y):
    if mp6_to_bool(x):
        return x
    return y()

def mp6_defined_or(x, y):
    if mp6_isa(x, 'Mu'):
        return y()
    return x

def mp6_isa(v, name):
    try:
        v = v.f_get()
    except AttributeError:
        None
    try:
        return v.f_isa(name)
    except AttributeError:
        if type(v) == type(1):
            return (name == 'Int') or (name == 'Num')
        if type(v) == type(1.1):
            return name == 'Num'
        if type(v) == type("a"):
            return name == 'Str'
        if type(v) == type(u"a"):
            return name == 'Str'
        f_warn("Warning: Can't calculate .isa() on ", v.__class__.__name__, " ", f_perl(v))
        return False

def mp6_join(l, s):
    try:
        l = l.f_get()
    except AttributeError:
        None
    if not mp6_isa(l, 'Array'):
        return unicode(l)
    return s.join(l.f_map( lambda x: unicode(x) ).l)

def mp6_split(l, s):
    try:
        l = l.f_get()
    except AttributeError:
        None
    return mp6_Array(unicode(l).split(s))

def mp6_index(s, s2):
    try:
        return s.index(s2)
    except ValueError:
        return -1

def mp6_to_num(s): 
    try:
        c = coerce(s, 0);
        return c[0]
    except TypeError:
        s = unicode(s)
        try:
            s.index(".")
            return float(s)
        except ValueError:
            try:
                return int(s)
            except ValueError:
                return 0
        except AttributeError:
            try:
                return int(s)
            except ValueError:
                return 0
            except TypeError:
                return 0

class mp6_Array:
    def __init__(self, l):
        self.l = l
    def __unicode__(self):
        return " ".join(map(lambda x: unicode(x), self.l))
    def __int__(self):
        return len(self.l)
    def f_bool(self):
        return len(self.l) > 0
    def __iter__(self):
        return self.l.__iter__()
    def f_perl(self, last_seen={}):
        seen = last_seen.copy()
        o = self.l
        try:
            if seen[id(o)]:
                return '*recursive*'
        except KeyError:
            None
        seen[id(o)] = True
        return "[" + ", ".join(map(lambda x: f_perl(x, seen), o)) + "]"
    def f_elems(self):
        return len(self.l)
    def f_push(self, s):
        try:
            self.l.append(s.f_get())
        except AttributeError:
            self.l.append(s)
    def f_unshift(self, s):
        try:
            self.l.insert(0, s.f_get())
        except AttributeError:
            self.l.insert(0, s)
    def f_pop(self):
        try:
            return self.l.pop()
        except IndexError:
            return mp6_Mu()
    def f_set(self, v):
        self.l = []
        v = v.l
        for x in range(0, len(v)):
            try:
                self.l.append(v[x].f_get())
            except AttributeError:
                self.l.append(v[x])
        return self
    def f_index_set(self, i, s):
        try:
            s = s.f_get()
        except AttributeError:
            None
        while True:
            try:
                self.l[i] = s
                return s
            except IndexError:
                self.l.append( mp6_Mu() )
    def f_shift(self):
        try:
            return self.l.pop(0)
        except IndexError:
            return mp6_Mu()
    def f_index(self, i):
        try:
            return mp6_Mu_get_proxy(self, i, self.l[i])
        except IndexError:
            return mp6_Mu_get_proxy(self, i, mp6_Mu())
    def f_isa(self, name):
        return name == 'Array'
    def str(self):
        return unicode(self.l)
    def f_map(self, f):
        result = []
        v = map(f, self.l)
        for x in range(0, len(v)):
            try:
                result.append(v[x].f_get())
            except AttributeError:
                result.append(v[x])
        return mp6_Array(result)
 
class mp6_Hash:
    def __init__(self, l):
        self.l = l
    def __unicode__(self):
        return unicode(self.l)
    def __int__(self):
        return len(self.l)
    def f_bool(self):
        return len(self.l) > 0
    def __iter__(self):
        return self.l.__iter__()
    def __getitem__(self, k):
        return self.l.__getitem__(k) 
    def f_perl(self, last_seen={}):
        seen = last_seen.copy()
        o = self.l
        try:
            if seen[id(o)]:
                return '*recursive*'
        except KeyError:
            None
        seen[id(o)] = True
        out = [];
        for i in o.keys():
            out.append(f_perl(i, seen) + " => " + f_perl(o[i], seen))
        return "{" + ", ".join(out) + "}";
    def values(self):
        return mp6_Array(self.l.values())
    def keys(self):
        return mp6_Array(self.l.keys())
    def f_pairs(self):
        out = [];
        for i in self.l.keys():
            out.append( Pair(v_key=i, v_value=self.__getitem__(i)) )
        return mp6_Array(out)
    def has_key(self, k):
        return self.l.has_key(k)
    def f_set(self, v):
        self.l = {}
        v = v.l
        for x in v.keys():
            try:
                self.l[x] = v[x].f_get()
            except AttributeError:
                self.l[x] = v[x]
        return self
    def f_index_set(self, i, s):
        try:
            i = i.f_get()
        except AttributeError:
            None
        try:
            self.l[i] = s.f_get()
        except AttributeError:
            self.l[i] = s
        return s
    def f_lookup(self, i):
        try:
            i = i.f_get()
        except AttributeError:
            None
        try:
            return mp6_Mu_get_proxy(self, i, self.l[i])
        except KeyError:
            return mp6_Mu_get_proxy(self, i, mp6_Mu())
    def f_isa(self, name):
        return name == 'Hash'
    def str(self):
        return unicode(self.l)

class mp6_Mu:
    def __unicode__(self):
        return ""
    def __int__(self):
        return 0
    def __float__(self):
        return 0.0
    def f_bool(self):
        return False
    def __add__(self, x):
        return x
    def __index__(self):
        return 0
    def __getitem__(self, k):
        return self 
    def __iter__(self):
        return [].__iter__()
    def f_perl(self, seen={}):
        return "Mu"
    def f_isa(self, name):
        return name == 'Mu'

class mp6_Scalar:
    def __init__(self, v=mp6_Mu()):
        self.v = v
    def __getattr__(self,name):
        return getattr(self.v, name)
    def __unicode__(self):
        return unicode(self.v)
    def f_perl(self, seen={}):
        return f_perl(self.v, seen)
    def f_id(self):
        return mp6_id(self.v)
    def f_bool(self):
        return mp6_to_bool(self.v)
    def f_set(self, v):
        try:
            self.v = v.f_get()
        except AttributeError:
            self.v = v
    def f_get(self):
        return self.v
    def f_lookup(self, i):
        try:
            return self.v.f_lookup(i)
        except AttributeError:
            if self.v.f_isa('Mu'):
                return mp6_Mu_lookup_proxy(self, i)
            return self.v.f_lookup(i)
    def f_index(self, i):
        try:
            return self.v.f_index(i)
        except AttributeError:
            if self.v.f_isa('Mu'):
                return mp6_Mu_index_proxy(self, i)
            return self.v.f_index(i)

class mp6_Mu_get_proxy(mp6_Mu):
    def __init__(self, parent, i, v):
        self.parent = parent
        self.i = i
        self.v = v
    def __getattr__(self,name):
        return getattr(self.v, name)
    def __unicode__(self):
        return unicode(self.v)
    def f_perl(self, seen={}):
        return f_perl(self.v, seen)
    def f_id(self):
        return mp6_id(self.v)
    def f_bool(self):
        return mp6_to_bool(self.v)
    def f_get(self):
        return self.v
    def f_set(self, v):
        self.v = v
        self.parent.f_index_set(self.i, v)
    def f_lookup(self, i):
        try:
            return self.v.f_lookup(i)
        except AttributeError:
            return mp6_Mu_lookup_proxy(self, i)
    def f_index(self, i):
        try:
            return self.v.f_index(i)
        except AttributeError:
            return mp6_Mu_index_proxy(self, i)
    def f_isa(self, name):
        return self.v.f_isa(name)

class mp6_Mu_index_proxy(mp6_Mu):
    def __init__(self, parent, i):
        self.parent = parent
        self.i = i
    def f_set(self, v):
        Array_a = mp6_Array([])
        Array_a.f_index_set(self.i, v)
        self.parent.f_set(Array_a)
    def f_lookup(self, i):
        return mp6_Mu_lookup_proxy(self, i)
    def f_index(self, i):
        return mp6_Mu_index_proxy(self, i)

class mp6_Mu_lookup_proxy(mp6_Mu):
    def __init__(self, parent, i):
        self.parent = parent
        self.i = i
    def f_set(self, v):
        Hash_a = mp6_Hash({})
        Hash_a.f_index_set(self.i, v)
        self.parent.f_set(Hash_a)
    def f_lookup(self, i):
        return mp6_Mu_lookup_proxy(self, i)
    def f_index(self, i):
        return mp6_Mu_index_proxy(self, i)

class mp6_Die(Exception):
    def __init__(self):
        None
    def f_isa(self, name):
        return name == 'Exception::Die'

class mp6_Return(Exception):
    def __init__(self, value=mp6_Mu()):
        self.value = value
    def f_isa(self, name):
        return name == 'Exception::Return'

class Perlito__Match:
    def __init__(self, **arg):
        self.v_m = mp6_Hash({})
        self.v_to = mp6_Scalar()
        self.v_capture = mp6_Scalar()
        self.v_to.f_set(0)
        for k in arg:
            self.__dict__[k] = mp6_Scalar()
            self.__dict__[k].f_set(arg[k])
    def f__setattr__(self, k, v):
        try:
            self.__dict__[k].f_set(v)
        except KeyError:
            self.__dict__[k] = mp6_Scalar()
            self.__dict__[k].f_set(v)
        return self.__dict__[k]
    def __unicode__(self):
        if mp6_to_bool(self.v_bool):
            if not(mp6_isa(self.v_capture,'Mu')): 
                return unicode(self.v_capture)
            return self.v_str[self.v_from:self.v_to]
        return u''
    def f_bool(self):
        return mp6_to_bool(self.v_bool)
    def f_set(self, m):
        self.v_m.f_set(m.v_m)
        self.v_to.f_set(m.v_to)
        self.v_str.f_set(m.v_str)
        self.v_from.f_set(m.v_from)
        self.v_bool.f_set(m.v_bool)
        self.v_capture.f_set(m.v_capture)
    def f_capture(self):
        return self.v_capture
    def f_index_set(self, k, v):
        return self.v_m.f_index_set(k, v)
    def f_lookup(self, k):
        return self.v_m.f_lookup(k)
    def f_scalar(self):
        if mp6_to_bool(self.v_bool):
            if not(mp6_isa(self.v_capture,'Mu')): 
                return self.v_capture
            return self.v_str[self.v_from:self.v_to]
        return self.v_capture
    def has_key(self, k):
        return self.v_m.has_key(k)
    def f_from(self):
        return self.v_from
    def f_to(self):
        return self.v_to
    def f_isa(self, name):
        return name == 'Match'
Perlito__Match_proto = Perlito__Match()
__builtin__.Perlito__Match = Perlito__Match
__builtin__.Perlito__Match_proto = Perlito__Match_proto


try:        
    type(Perlito__Grammar)  
except NameError:
    class Perlito__Grammar:
        def __init__(self, **arg):
            self.__dict__.update(arg)
        def f_word(self, s, pos):
            m = re.match( r"\w", s[pos:] )
            if m:
                return Perlito__Match(v_str=s, v_from=pos, v_to=m.end() + pos, v_bool=1 )
            return Perlito__Match(v_bool=0)
        def f_digit(self, s, pos):
            m = re.match( r"\d", s[pos:] )
            if m:
                return Perlito__Match(v_str=s, v_from=pos, v_to=m.end() + pos, v_bool=1 )
            return Perlito__Match(v_bool=0)
        def f_space(self, s, pos):
            m = re.match( r"\s", s[pos:] )
            if m:
                return Perlito__Match(v_str=s, v_from=pos, v_to=m.end() + pos, v_bool=1 )
            return Perlito__Match(v_bool=0)
        def f_isa(self, name):
            return name == 'Grammar'
Perlito__Grammar_proto = Perlito__Grammar()
__builtin__.Perlito__Grammar = Perlito__Grammar
__builtin__.Perlito__Grammar_proto = Perlito__Grammar_proto


class Main:
    def __init__(self, **arg):
        self.__dict__.update(arg)
    def f__setattr__(v_self, k, v):
        v_self.__dict__[k] = v
        return v_self.__dict__[k]
    def f_bool(self):
        return 1
    def f_isa(v_self, name):
        return name == 'Main'
    def f_lisp_escape_string(self, s):
        o = s.replace( "\\", "\\\\");
        o = o.replace( '"', "\\\"");
        return o;
    def f_to_javascript_namespace(self, s):
        o = s.replace( "::", "$");
        return o;
    def f_to_lisp_namespace(self, s):
        o = s.replace( "::", "-");
        return "mp-" + o;
    def f_to_go_namespace(self, s):
        o = s.replace( "::", "__");
        return o
Main_proto = Main()
__builtin__.Main = Main
__builtin__.Main_proto = Main_proto


class IO:
    def __init__(v_self, **arg):
        v_self.__dict__.update(arg)
    def f__setattr__(v_self, k, v):
        v_self.__dict__[k] = v
        return v_self.__dict__[k]
    def f_bool(self):
        return 1
    def f_isa(v_self, name):
        return name == 'IO'
    def f_slurp(self, name):
        return file(unicode(name)).read().decode('utf-8')
IO_proto = IO()
__builtin__.IO = IO
__builtin__.IO_proto = IO_proto


def _dump(o, seen):
    try:
        if seen[id(o)]:
            return '*recursive*'
    except KeyError:
        None
    seen[id(o)] = True
    out = [];
    for i in o.__dict__.keys():
        out.append(i[2:] + " => " + f_perl(o.__dict__[i], seen))
    name = o.__class__.__name__.replace( "__", "::")
    return name + ".new(" + ", ".join(out) + ")"

def mp6_id(o):
    try:
        return o.f_id()
    except AttributeError:
        return id(o)

def f_perl(o, last_seen={}):
    seen = last_seen.copy()
    try:
        return o.f_perl(seen)
    except AttributeError:
        if type(o) == type(False):
            return unicode(o)
        if type(o) == type(1):
            return unicode(o)
        if type(o) == type(1.1):
            return unicode(o)
        if type(o) == type("a"):
            return '"' + o + '"'
        if type(o) == type(u"a"):
            return u'"' + o + u'"'
        if type(o) == type([]):
            try:
                if seen[id(o)]:
                    return '*recursive*'
            except KeyError:
                None
            seen[id(o)] = True
            return "PythonArray([" + ", ".join(map(lambda x: f_perl(x, seen), o)) + "])"
        if type(o) == type({}):
            try:
                if seen[id(o)]:
                    return '*recursive*'
            except KeyError:
                None
            seen[id(o)] = True
            out = [];
            for i in o.keys():
                out.append(i + " => " + f_perl(o[i], seen))
            return "PythonHash({" + ", ".join(out) + "})";
        return _dump(o, seen)
__builtin__.f_perl = f_perl

__builtin__.List_ARGS = mp6_Array([])
List_ARGS.l.extend( map(lambda x: x.decode("utf-8"), sys.argv[1:]) )

