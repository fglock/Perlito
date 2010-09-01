# Perlito/Ruby/Runtime.rb
# 
# DESCRIPTION
# 
# Provides runtime routines for the Perlito-in-Ruby compiled code
# 
# AUTHORS
# 
# The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.
# 
# COPYRIGHT
# 
# Copyright 2010 by Flavio Soibelmann Glock and others.
# 
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.
# 
# See L<http://www.perl.com/perl/misc/Artistic.html>


class C_Main
end
$Main = C_Main.new()


def mp6_to_num (v)
    if v.class == String
        if v.index(".")
            return v.to_f
        end
        return v.to_i
    end
    if v == nil
        return 0
    end
    return v
end

def mp6_to_bool (v)
    if v.class == String
        return v != "0" && v != ""
    end
    if v.class == Fixnum || v.class == Float || v.class == Bignum
        return v != 0
    end
    if v.class == Array 
        return v.length != 0
    end
    if v.class == C_Perlito__Match
        return mp6_to_bool(v.v_bool)
    end
    return v
end

def mp6_to_scalar (v)
    if v.class == C_Perlito__Match
        return v.f_scalar()
    end
    if v.class == Array 
        return v.length
    end
    return v
end

def mp6_isa(o, s)
    class_name = o.class.to_s.sub("__", "::").sub("C_","")
    if s == 'Str'
        return class_name == 'String'
    end
    if s == 'Int'
        return class_name == 'Fixnum' || class_name == 'Bignum'
    end
    if s == 'Num'
        return class_name == 'Float'
    end
    return class_name == s
end

class C_Perlito__Match < Hash
    $Perlito__Match = C_Perlito__Match.new()
    namespace = $Perlito__Match
    attr_accessor :v_from
    def f_from()
        return self.v_from
    end
    attr_accessor :v_to
    def f_to()
        return self.v_to
    end
    attr_accessor :v_capture
    def f_capture()
        return self.v_capture
    end
    attr_accessor :v_str
    def f_str()
        return self.v_str
    end
    attr_accessor :v_bool
    def f_bool()
        return self.v_bool
    end
    def f_scalar()
        if mp6_to_bool(self.v_bool)
            if self.v_capture != nil
                return self.v_capture
            else
                return self.v_str[self.v_from .. self.v_to-1]
            end
        end
        return nil
    end
    def to_s()
        return self.f_scalar().to_s()
    end
end

class C_Perlito__Grammar
    $Perlito__Grammar = C_Perlito__Grammar.new()
    namespace = $Perlito__Grammar
    def f_word(s, pos)
        /^(\w)/.match(s[pos,1])
        m = C_Perlito__Match.new
        if $~
            m.v_str  = s
            m.v_from = pos
            m.v_to   = $~.end(1) + pos
            m.v_bool = true
        else
            m.v_bool = false
        end
        return m
    end
    def f_digit(s, pos)
        /^(\d)/.match(s[pos,1])
        m = C_Perlito__Match.new
        if $~
            m.v_str  = s
            m.v_from = pos
            m.v_to   = $~.end(1) + pos
            m.v_bool = true
        else
            m.v_bool = false
        end
        return m
    end
    def f_space(s, pos)
        /^(\s)/.match(s[pos,1])
        m = C_Perlito__Match.new
        if $~
            m.v_str  = s
            m.v_from = pos
            m.v_to   = $~.end(1) + pos
            m.v_bool = true
        else
            m.v_bool = false
        end
        return m
    end
    def f_is_newline(s, pos)
        /^(\r\n?|\n\r?)/.match(s[pos,2])
        m = C_Perlito__Match.new
        if $~
            m.v_str  = s
            m.v_from = pos
            m.v_to   = $~.end(1) + pos
            m.v_bool = true
        else
            m.v_bool = false
        end
        return m
    end
    def f_not_newline(s, pos)
        /^(\r|\n)/.match(s[pos,1])
        m = C_Perlito__Match.new
        if $~
            m.v_bool = false
        else
            m.v_str  = s
            m.v_from = pos
            m.v_to   = pos + 1
            m.v_bool = true
        end
        return m
    end
end

def _dump(o)
    class_name = o.class.to_s.sub("__", "::").sub("C_","")
    attrs = ( o.methods.grep /^v_.+=/ ).map{ |x| 
                meth = x.to_s.sub("=",""); name = meth.sub("v_",""); (name + " => " + mp6_perl(o.send(meth))) 
            }.join(", ")
    return class_name + ".new(" + attrs + ")";
end
 
def mp6_perl(o)
    if o == false
        return 'False'
    end
    if o == true
        return 'True'
    end
    if o == nil
        return 'undef'
    end
    if o.class == String
        return "'" + $Main.f_perl_escape_string(o) + "'"   
    end
    if o.class == Fixnum || o.class == Float || o.class == Bignum
        return o.to_s
    end
    if o.class == Array 
        return "[" + (o.map{|x| mp6_perl(x)}).join(", ") + "]"
    end
    if o.class == Hash
        out = []
        for i in o.keys()
            out.push(i + " => " + mp6_perl(o[i]))
        end
        return "{" + out.join(", ") + "}";
    end
    return _dump(o)
end

class IO
    $IO = IO.new(1, "w")
    def f_slurp(s)
        IO.read(s)
    end
end

class C_Main
    def f_newline()
        return "\n"
    end
    def f_lisp_escape_string(s)
        o = s.gsub( "\\", "\\\\\\");
        o = o.gsub( '"',  "\\\\\"");
        return o;
    end
    def f_javascript_escape_string(s)
        o = s.gsub( "\\", "\\\\\\");
        o = o.gsub( '"',  "\\\\\"");
        o = o.gsub( "\n", "\\\\n");
        return o;
    end
    def f_perl_escape_string(s)
        o = s.gsub( "\\", "\\\\\\")
        o = o.gsub( "'",  "\\\\\'")
        return o
    end
    def f_to_javascript_namespace(s)
        o = s.gsub( "::", "$");
        return o;
    end
    def f_to_lisp_namespace(s)
        o = s.gsub( "::", "-");
        return "mp-" + o;
    end
    def f_to_go_namespace(s)
        o = s.gsub( "::", "__");
        return o
    end
end
