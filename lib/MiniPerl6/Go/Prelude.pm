use v6;

class MiniPerl6::Match {
    has $.from;
    has $.to;
    has $.str;
    has $.bool;
    has $.capture;
    has %.hash;
    has @.array;

    method perl {
        return "::Match("
            ~ "from => "    ~ $.from.perl
            ~ ", to => "    ~ $.to.perl
            ~ ", bool => "  ~ ($.bool.bool).perl
            ~ ", capture => " ~ $.capture.perl
            ~ ", hash => "  ~ $.hash.perl
            ~ ", array => " ~ $.array.perl
            ~ ")";
    }

    method Bool         { $.bool.Bool }

    # Note: These methods are just stubs.
    #       The actual methods are implemented in Runtime.go

    method Str          { die "TODO" }
    method scalar       { die "TODO" }
}

class MiniPerl6::Grammar {

    # Note: These tokens are just stubs.
    #       The actual methods are implemented in Runtime.go

    token is_newline    { . }
    token word          { . }
    token digit         { . }
    token not_newline   { . }
    token space         { . }
}

class IO {
    sub slurp           { die "stub" }
}

class Main {

    method newline      { "\n" }

    sub to_lisp_identifier ( $ident ) {
        return 'sv-' ~ $ident;
    }
    sub lisp_dump_object ( $class_name, $data ) {
        return $class_name ~ '( ' ~ ($data.>>perl).join(', ') ~ ' )';
    }

    # Note: These methods are just stubs.
    #       The actual methods are implemented in Runtime.go

    sub to_go_namespace          { die "TODO" }
    sub to_javascript_namespace  { die "TODO" }
    sub javascript_escape_string { die "TODO" }
    sub to_lisp_namespace        { die "TODO" }
    sub lisp_escape_string       { die "TODO" }
    sub perl_escape_string       { die "TODO" }
}

=begin

=head1 NAME 

MiniPerl6::Go::Prelude - Runtime for MiniPerl6-in-Go

=head1 SYNOPSIS

=head1 DESCRIPTION

This module contains MiniPerl6 code for the MiniPerl6-in-Go runtime.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http:#dev.perl.org/perl6>.

The Pugs homepage at L<http:#pugscode.org/>.

=head1 COPYRIGHT

Copyright 2009 by Flavio Soibelmann Glock.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
