"""
miniperl6.python.runtime

DESCRIPTION

Provides runtime routines for the MiniPerl6-in-Python compiled code

AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

COPYRIGHT

Copyright 2010 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

"""

import sys
import re
import __builtin__

__all__ = ['mp6_print', 'mp6_say', 'mp6_warn', 
           'mp6_to_num', 'mp6_to_scalar', 'mp6_to_bool', 'mp6_isa',
           'mp6_join', 'mp6_index', 'mp6_perl',
           'mp6_Undef', 'mp6_Array', 'mp6_Hash',
           'mp6_Return',
           'MiniPerl6__Match',
           'MiniPerl6__Grammar', 'MiniPerl6__Grammar_proto', 
           'Main', 'Main_proto',
           'IO', 'IO_proto']

def mp6_print(*msg):
    for m in msg:
        sys.stdout.write(str(m))

def mp6_say(*msg):
    for m in msg:
        sys.stdout.write(str(m))
    sys.stdout.write("\n")

def mp6_warn(*msg):
    for m in msg:
        sys.stderr.write(str(m))
    sys.stderr.write("\n")

def mp6_to_scalar(v):
    try:
        return v.f_scalar()
    except AttributeError:
        return v

def mp6_to_bool(o):
    if type(o) == type(False):
        return o
    if type(o) == type(1):
        return o != 0
    if type(o) == type(1.1):
        return o != 0
    if type(o) == type("aa"):
        return o != "" and o != "0"
    if type(o) == type([]):
        return len(o) != 0
    if type(o) == type({}):
        return len(o.keys) != 0
    if type(o) == type(None):
        return False
    return o.__nonzero__()

def mp6_isa(v, name):
    try:
        return v.f_isa(name)
    except AttributeError:
        if name == 'Int' and type(v) == type(1):
            return True
        if name == 'Num' and type(v) == type(1.1):
            return True
        if name == 'Str' and type(v) == type("aa"):
            return True
        print "Warning: Can't calculate .isa() on", v
        return False

def mp6_join(l, s):
    return s.join(l)

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
    def __str__(self):
        return str(self.l)
    def __int__(self):
        return len(self.l)
    def __nonzero__(self):
        return len(self.l) > 0
    def __iter__(self):
        return self.l.__iter__()
    def f_perl(self):
        return mp6_perl(self.l)
    def f_extend(self, l):
        try:
            self.l.extend(l.l)
        except AttributeError:
            self.l.extend(l)
    def f_push(self, s):
        self.l.append(s)
    def f_pop(self):
        try:
            return self.l.pop()
        except IndexError:
            return mp6_Undef()
    def f_set(self, i, s):
        while True:
            try:
                self.l[i] = s
                return s
            except IndexError:
                self.l.append( mp6_Undef() )
    def f_shift(self):
        try:
            return self.l.pop(0)
        except IndexError:
            return mp6_Undef()
    def f_index(self, i):
        try:
            return self.l[i]
        except IndexError:
            return mp6_Undef()
    def f_isa(self, name):
        return name == 'Array'
    def str(self):
        return str(self.l)


class mp6_Hash:
    def __init__(self, l):
        self.l = l
    def __str__(self):
        return str(self.l)
    def __int__(self):
        return len(self.l)
    def __nonzero__(self):
        return len(self.l) > 0
    def __iter__(self):
        return self.l.__iter__()
    def __getitem__(self, k):
        return self.l.__getitem__(k) 
    def f_perl(self):
        return mp6_perl(self.l)
    def values(self):
        return self.l.values()
    def keys(self):
        return self.l.keys()
    def has_key(self, k):
        return self.l.has_key(k)
    def f_set(self, i, s):
        self.l[i] = s
        return s
    def f_lookup(self, i):
        try:
            return self.l[i]
        except KeyError:
            return mp6_Undef()
    def f_isa(self, name):
        return name == 'Hash'
    def str(self):
        return str(self.l)


class mp6_Mu:
    def __str__(self):
        return ""
    def __int__(self):
        return 0
    def __float__(self):
        return 0.0
    def __nonzero__(self):
        return False
    def __add__(self, x):
        return x
    def __index__(self):
        return 0
    def __getitem__(self, k):
        return self 
    def f_perl(self):
        return "Mu"
    def f_isa(self, name):
        return name == 'Mu'


class mp6_Return(Exception):
    def __init__(self, value):
        self.value = value
    def f_isa(self, name):
        return name == 'Exception::Return'

class MiniPerl6__Match:
    def __init__(self, **arg):
        self.v_m = {}
        self.v_to = 0
        self.__dict__.update(arg)
    def __setattr__(v_self, k, v):
        v_self.__dict__[k] = v
        return v
    def __str__(self):
        if mp6_to_bool(self.v_bool):
            try:
                return str(self.v_capture)
            except AttributeError:
                return self.v_str[self.v_from:self.v_to]
        return ''
    def __nonzero__(self):
        return mp6_to_bool(self.v_bool)
    def __setitem__(self, k, v):
        self.v_m[k] = v
    def f_set(self, k, v):
        self.v_m[k] = v
        return v
    def f_lookup(self, k):
        try:
            return self.v_m[k]
        except KeyError:
            return mp6_Undef()
    def __getitem__(self, k):
        return self.v_m[k] 
    def f_scalar(self):
        if mp6_to_bool(self.v_bool):
            try:
                return self.v_capture
            except AttributeError:
                return self.v_str[self.v_from:self.v_to]
        return mp6_Undef()
    def has_key(self, k):
        return self.v_m.has_key(k)
    def f_from(self):
        return self.v_from
    def f_to(self):
        return self.v_to
    def f_isa(self, name):
        return name == 'Match'


try:        
    type(MiniPerl6__Grammar)  
except NameError:
    class MiniPerl6__Grammar:
        def __init__(self, **arg):
            self.__dict__.update(arg)
        def f_word(self, s, pos):
            m = re.match( r"\w", s[pos:] )
            if m:
                return MiniPerl6__Match(v_str=s, v_from=pos, v_to=m.end() + pos, v_bool=1 )
            return MiniPerl6__Match(v_bool=0)
        def f_digit(self, s, pos):
            m = re.match( r"\d", s[pos:] )
            if m:
                return MiniPerl6__Match(v_str=s, v_from=pos, v_to=m.end() + pos, v_bool=1 )
            return MiniPerl6__Match(v_bool=0)
        def f_space(self, s, pos):
            m = re.match( r"\s", s[pos:] )
            if m:
                return MiniPerl6__Match(v_str=s, v_from=pos, v_to=m.end() + pos, v_bool=1 )
            return MiniPerl6__Match(v_bool=0)
        def f_is_newline(self, s, pos):
            m = re.match( r"\r\n?|\n\r?", s[pos:] )
            if m:
                return MiniPerl6__Match(v_str=s, v_from=pos, v_to=m.end() + pos, v_bool=1 )
            return MiniPerl6__Match(v_bool=0)
        def f_not_newline(self, s, pos):
            m = re.match( r"\r|\n", s[pos:] )
            if m:
                return MiniPerl6__Match(v_bool=0)
            return MiniPerl6__Match(v_str=s, v_from=pos, v_to=pos+1, v_bool=1 )
        def f_isa(self, name):
            return name == 'Grammar'
MiniPerl6__Grammar_proto = MiniPerl6__Grammar()


class Main:
    def __init__(self, **arg):
        self.__dict__.update(arg)
    def __setattr__(v_self, k, v):
        v_self.__dict__[k] = v
    def __nonzero__(self):
        return 1
    def f_isa(v_self, name):
        return name == 'Main'
    def f_newline(self):
        return "\n"
    def f_lisp_escape_string(self, s):
        o = s.replace( "\\", "\\\\");
        o = o.replace( '"', "\\\"");
        return o;
    def f_javascript_escape_string(self, s):
        o = s.replace( "\\", "\\\\");
        o = o.replace( '"', "\\\"");
        o = o.replace( "\n", "\\n");
        return o;
    def f_perl_escape_string(self, s):
        o = s.replace( "\\", "\\\\")
        o = o.replace( "'", "\\'")
        return o
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
    def __setattr__(v_self, k, v):
        v_self.__dict__[k] = v
    def __nonzero__(self):
        return 1
    def f_isa(v_self, name):
        return name == 'IO'
    def f_slurp(self, name):
        return file(name).read()
IO_proto = IO()
__builtin__.IO = IO
__builtin__.IO_proto = IO_proto


def _dump(o):
    out = [];
    for i in o.__dict__.keys():
        out.append(i[2:] + " => " + mp6_perl(o.__dict__[i]))
    name = o.__class__.__name__.replace( "__", "::");
    return name + ".new(" + ", ".join(out) + ")";

def mp6_perl(o):
    try:
        return o.f_perl()
    except AttributeError:
        if type(o) == type(False):
            return str(o)
        if type(o) == type(1):
            return str(o)
        if type(o) == type(1.1):
            return str(o)
        if type(o) == type("aa"):
            return '"' + Main_proto.f_javascript_escape_string(o) + '"'
        if type(o) == type([]):
            return "[" + ", ".join(map(lambda x: mp6_perl(x), o)) + "]"
        if type(o) == type({}):
            out = [];
            for i in o.keys():
                out.append(i + " => " + mp6_perl(o[i]))
            return "{" + ", ".join(out) + "}";
        return _dump(o)

__builtin__.List_ARGS = [mp6_Array([])]
List_ARGS[0].l.extend(sys.argv[1:])

