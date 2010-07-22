# MiniPerl6/Ruby/Runtime.rb
# 
# DESCRIPTION
# 
# Provides runtime routines for the MiniPerl6-in-Ruby compiled code
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

class Mp6_Array < Array
end

def mp6_to_num (v)
    if v.class == "".class
        if v.index(".")
            return v.to_f
        end
        return v.to_i
    end
    return v
end

def mp6_to_bool (v)
    if v.class == "".class
        return v != "0" && v != ""
    end
    if v.class == 1.class
        return v != 0
    end
    if v.class == [].class || v.class == Mp6_Array
        return v.length != 0
    end
    return v
end

def mp6_to_scalar (v)
    # TODO
end

class MiniPerl6__Match
    $MiniPerl6__Match = MiniPerl6__Match.new()
    namespace = $MiniPerl6__Match
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
end

