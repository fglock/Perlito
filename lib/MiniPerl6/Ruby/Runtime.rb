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
    return v
end
