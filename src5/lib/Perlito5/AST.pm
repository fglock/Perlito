use v5;

class CompUnit {
    sub name { $_[0]->{'name'} }
    sub body { $_[0]->{'body'} }
}

class Val::Int {
    sub int { $_[0]->{'int'} }
}

class Val::Num {
    sub num { $_[0]->{'num'} }
}

class Val::Buf {
    sub buf { $_[0]->{'buf'} }
}

class Lit::Block {
    sub sig { $_[0]->{'sig'} }
    sub stmts { $_[0]->{'stmts'} }
}

class Lit::Array {
    sub array1 { $_[0]->{'array1'} }
}

class Lit::Hash {
    sub hash1 { $_[0]->{'hash1'} }
}

class Index {
    sub obj { $_[0]->{'obj'} }
    sub index_exp { $_[0]->{'index_exp'} }
}

class Lookup {
    sub obj { $_[0]->{'obj'} }
    sub index_exp { $_[0]->{'index_exp'} }
}

class Var {
    sub sigil { $_[0]->{'sigil'} }
    sub twigil { $_[0]->{'twigil'} }
    sub namespace { $_[0]->{'namespace'} }
    sub name { $_[0]->{'name'} }
}

class Proto {
    sub name { $_[0]->{'name'} }
}

class Call {
    sub invocant { $_[0]->{'invocant'} }
    sub method { $_[0]->{'method'} }
    sub arguments { $_[0]->{'arguments'} }
}

class Apply {
    sub code { $_[0]->{'code'} }
    sub arguments { $_[0]->{'arguments'} }
    sub namespace { $_[0]->{'namespace'} }
}

class If {
    sub cond { $_[0]->{'cond'} }
    sub body { $_[0]->{'body'} }
    sub otherwise { $_[0]->{'otherwise'} }
}

class While {
    sub init { $_[0]->{'init'} }
    sub cond { $_[0]->{'cond'} }
    sub continue { $_[0]->{'continue'} }
    sub body { $_[0]->{'body'} }
}

class For {
    sub cond { $_[0]->{'cond'} }
    sub body { $_[0]->{'body'} }
}

class Decl {
    sub decl { $_[0]->{'decl'} }
    sub type { $_[0]->{'type'} }
    sub var { $_[0]->{'var'} }
}

class Sig {
    sub invocant { $_[0]->{'invocant'} }
    sub positional { $_[0]->{'positional'} }
    sub named { $_[0]->{'named'} }
}

class Sub {
    sub name { $_[0]->{'name'} }
    sub sig { $_[0]->{'sig'} }
    sub block { $_[0]->{'block'} }
}

class Do {
    sub block { $_[0]->{'block'} }
}

class Use {
    sub mod { $_[0]->{'mod'} }
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
