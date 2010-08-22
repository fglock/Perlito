use v6;

# This module answers the question - does this sub/method need a try block to return() to?

class CompUnit {
    has $.name;
    has %.attributes;
    has %.methods;
    has @.body;
    method visit {} 
}

class Val::Int {
    method visit {}
}
class Val::Bit {
    method visit {}
}
class Val::Num {
    method visit {}
}
class Val::Buf {
    method visit {}
}
class Val::Undef {
    method visit {}
}
class Lit::Array {
    method visit {}
}
class Lit::Hash {
    method visit {} 
}
class Lit::Object {
    method visit {} 
}
class Index {
    method visit {} 
}
class Lookup {
    method visit {} 
}
class Var {
    method visit {}
}
class Bind {
    method visit {} 
}
class Proto {
    method visit {} 
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    method visit {} 
}

class Apply {
    has $.code;
    has @.arguments;
    has $.namespace;
    method visit {} 
}

class Return {
    has $.result;
    method visit {
    }
}

class If {
    has $.cond;
    has @.body;
    has @.otherwise;
    method visit {} 
}


class While {
    has $.cond;
    has @.body;
    method visit {
    }
}

class For {
    has $.cond;
    has @.body;
    has @.topic;
    method visit {
    }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method visit {
    }
}

class Method {
    has $.name;
    has $.sig;
    has @.block;
    method visit {
    }
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method visit {
    }
}

class Do {
    has @.block;
    method visit {
    }
}

class Use {
    has $.mod;
    method visit {
    }
}

=begin

=head1 NAME 

MiniPerl6::AST - AST processor

=head1 SYNOPSIS

    $program.visit( ... )  

=head1 DESCRIPTION

This module processes AST trees for the MiniPerl6 compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006, 2010 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
