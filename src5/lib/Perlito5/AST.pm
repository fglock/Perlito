use v5;

class CompUnit {
    has $.name;
    has $.body;
}

class Val::Int {
    has $.int;
}

class Val::Bit {
    has $.bit;
}

class Val::Num {
    has $.num;
}

class Val::Buf {
    has $.buf;
}

class Lit::Block {
    has $.sig;
    has $.stmts;
}

class Lit::Array {
    has $.array1;
}

class Lit::Hash {
    has $.hash1;
}

class Index {
    has $.obj;
    has $.index_exp;
}

class Lookup {
    has $.obj;
    has $.index_exp;
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.namespace;
    has $.name;
}

class Proto {
    has $.name;
}

class Call {
    has $.invocant;
    has $.method;
    has $.arguments;
}

class Apply {
    has $.code;
    has $.arguments;
    has $.namespace;
}

class If {
    has $.cond;
    has $.body;
    has $.otherwise;
}

class While {
    has $.init;
    has $.cond;
    has $.continue;
    has $.body;
}

class For {
    has $.cond;
    has $.body;
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
}

class Sig {
    has $.invocant;
    has $.positional;
    has $.named;
}

class Sub {
    has $.name;
    has $.sig;
    has $.block;
}

class Do {
    has $.block;
}

class Use {
    has $.mod;
}

=begin

=head1 NAME

Perlito5::AST - Base class for Perlito AST nodes

=head1 SYNOPSIS

    # TODO

=head1 DESCRIPTION

This module provides AST node class declarations for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

=head1 COPYRIGHT

Copyright 2011 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
