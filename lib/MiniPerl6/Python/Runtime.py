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

__all__ = ['mp6_print', 'mp6_say', 'mp6_warn', 
           'mp6_to_num', 'mp6_to_scalar', 'mp6_isa',
           'mp6_Undef', 'mp6_Array', 
           'mp6_Return',
           'MiniPerl6__Match',
           'MiniPerl6__Grammar', 'MiniPerl6__Grammar_proto']

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

def mp6_isa(v, name):
    try:
        return v.f_isa(name)
    except AttributeError:
        print "Warning: Can't calculate .isa() on", v
        return False

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
    def f_extend(self, l):
        try:
            self.l.extend(l.l)
        except AttributeError:
            self.l.extend(l)
    def f_push(self, s):
        self.l.append(s)
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


class mp6_Undef:
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
    def f_isa(self, name):
        return name == 'Undef'


class mp6_Return(Exception):
    def __init__(self, value):
        self.value = value
    def f_isa(self, name):
        return name == 'Exception::Return'

class MiniPerl6__Match:
    def __init__(self, **arg):
        self.match = {}
        self.v_to = 0
        for kw in arg.keys():
            self.__dict__.update({'v_' + kw:arg[kw]})
    def __setattr__(v_self, k, v):
        v_self.__dict__[k] = v
    def __str__(self):
        if self.v_bool:
            return self.v_str[self.v_c_from:self.v_to]
        return ''
    def __nonzero__(self):
        return self.v_bool
    def __setitem__(self, k, v):
        self.match[k] = v
    def __getitem__(self, k):
        return self.match[k] 
    def __repr__(self):
        capture = ['']
        try:
            capture[0] = ", capture => " + str(self.v_capture)
        except AttributeError:
            capture[0] = ", capture => undef"
        return "Match.new(from => " + str(self.f_from()) + ", to => " + str(self.f_to()) + ", bool => " + str(self.v_bool) + capture[0] + ")" 
    def f_scalar(self):
        if self.v_bool:
            try:
                return self.v_capture
            except AttributeError:
                return self.v_str[self.v_c_from:self.v_to]
        return mp6_Undef()
    def has_key(self, k):
        return self.match.has_key(k)
    def f_from(self):
        return self.v_c_from
    def f_to(self):
        return self.v_to
    def f_isa(self, name):
        return name == 'Match'


try:        
    type(MiniPerl6__Grammar)  
except NameError:
    class MiniPerl6__Grammar:
        def __init__(self, **arg):
            for kw in arg.keys():
                self.__dict__.update({'v_' + kw:arg[kw]})
        def f_word(self, s, pos):
            m = re.match( r"\w", s[pos:] )
            if m:
                return MiniPerl6__Match(str=s, c_from=pos, to=m.end() + pos, bool=1 )
            return MiniPerl6__Match(bool=0)
        def f_digit(self, s, pos):
            m = re.match( r"\d", s[pos:] )
            if m:
                return MiniPerl6__Match(str=s, c_from=pos, to=m.end() + pos, bool=1 )
            return MiniPerl6__Match(bool=0)
        def f_space(self, s, pos):
            m = re.match( r"\s", s[pos:] )
            if m:
                return MiniPerl6__Match(str=s, c_from=pos, to=m.end() + pos, bool=1 )
            return MiniPerl6__Match(bool=0)
        def f_is_newline(self, s, pos):
            m = re.match( r"\r\n?|\n\r?", s[pos:] )
            if m:
                return MiniPerl6__Match(str=s, c_from=pos, to=m.end() + pos, bool=1 )
            return MiniPerl6__Match(bool=0)
        def f_isa(self, name):
            return name == 'Grammar'
MiniPerl6__Grammar_proto = MiniPerl6__Grammar()


