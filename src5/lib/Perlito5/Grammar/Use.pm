
package Perlito5::Grammar::Use;

use Perlito5::Precedence;
use Perlito5::Grammar;

Perlito5::Precedence::add_term( 'no'  => sub { Perlito5::Grammar::Use->term_use($_[0], $_[1]) } );
Perlito5::Precedence::add_term( 'use' => sub { Perlito5::Grammar::Use->term_use($_[0], $_[1]) } );

my $perl5lib    = './src5/lib';
my %module_seen;


token use_decl { 'use' | 'no' };

token term_use {
    <use_decl> <.Perlito5::Grammar.ws>
        <Perlito5::Grammar.full_ident>  [ - <Perlito5::Grammar.ident> ]? <Perlito5::Expression.list_parse>
        {
            my $ast = Perlito5::AST::Use->new(
                    code => $MATCH->{"use_decl"}->flat(),
                    mod  => $MATCH->{"Perlito5::Grammar.full_ident"}->flat()
                );

            parse_time_eval($ast);

            $MATCH->{"capture"} = [ 'term', $ast ];
        }
};

sub parse_time_eval {
    my $self = shift;
    if ($self->mod eq 'strict') {
        if ($self->code eq 'use') {
            Perlito5::strict->import();
        }
        elsif ($self->code eq 'no') {
            Perlito5::strict->unimport();
        }
    }
}

sub emit_time_eval {
    my $self = shift;
    if ($self->mod eq 'strict') {
        if ($self->code eq 'use') {
            Perlito5::strict->import();
        }
        elsif ($self->code eq 'no') {
            Perlito5::strict->unimport();
        }
    }
}

sub modulename_to_filename {
    my $s = shift;
    return Perlito5::Runtime::_replace( $s, '::', '/' );
}

sub expand_use {
    my $comp_units = shift;
    my $stmt = shift;

    my $module_name = $stmt->mod;
    return
        if $module_name eq 'v5'
        || $module_name eq 'strict'
        || $module_name eq 'feature';
    if (!($module_seen{$module_name})) {
        $module_seen{$module_name} = 1;
        # say "  now use: ", $module_name;
     
        # TODO - look for a precompiled version
        # build the filename
        my $filename = $module_name;
        $filename = $perl5lib . '/' . modulename_to_filename($filename) . '.pm';
        # warn "// now loading: ", $filename;
        # load source
        my $source = Perlito5::IO::slurp( $filename );

        # compile; push AST into comp_units
        # warn $source;
        my $m = Perlito5::Grammar->exp_stmts($source, 0);
        die "Syntax Error near ", $m->{"to"}
            if $m->{"to"} != length($source);
        push @$comp_units, @{ add_comp_unit(
            [
                Perlito5::AST::CompUnit->new(
                    name => 'main',
                    body => $m->flat(),
                )
            ]
        ) };
    }
}

sub add_comp_unit {
    my $parse = shift;
    my $comp_units = [];

    for my $comp_unit (@$parse) {
        if ($comp_unit->isa('Perlito5::AST::Use')) {
            expand_use($comp_units, $comp_unit);
        }
        elsif ($comp_unit->isa('Perlito5::AST::CompUnit')) {
            # warn "parsed comp_unit: '", $comp_unit->name, "'";
            for my $stmt (@{ $comp_unit->body }) {
                if ($stmt->isa('Perlito5::AST::Use')) {
                    expand_use($comp_units, $stmt);
                }
            }
        }
        push @$comp_units, $comp_unit;
        # say "comp_unit done";
    }
    return $comp_units;
}

1;

=begin

=head1 NAME

Perlito5::Grammar::Use - Parser and AST generator for Perlito

=head1 SYNOPSIS

    term_use($str)

=head1 DESCRIPTION

This module parses source code for Perl 5 statements and generates Perlito5 AST.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2010, 2011, 2012 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

