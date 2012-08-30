
package Perlito5::Grammar::Use;

use Perlito5::Precedence;
use Perlito5::Grammar;

Perlito5::Precedence::add_term( 'no'  => sub { Perlito5::Grammar::Use->term_use($_[0], $_[1]) } );
Perlito5::Precedence::add_term( 'use' => sub { Perlito5::Grammar::Use->term_use($_[0], $_[1]) } );


my %Perlito_internal_module = (
    strict   => 'Perlito5::strict',
    warnings => 'Perlito5::warnings',
    utf8     => 'Perlito5::utf8',
    bytes    => 'Perlito5::bytes',
    encoding => 'Perlito5::encoding',
);


token use_decl { 'use' | 'no' };

token term_use {
    <use_decl> <.Perlito5::Grammar::Space.ws>
    [
        <Perlito5::Grammar.val_version>
        {
            # TODO - check version

            $MATCH->{capture} = [ 'term',
                Perlito5::AST::Apply->new(
                    code => 'undef',
                    namespace => '',
                    arguments => []
                )
            ];
        }
    |
        <Perlito5::Grammar.full_ident>  [ - <Perlito5::Grammar.ident> ]? <Perlito5::Expression.list_parse>
        {

            my $list = Perlito5::Match::flat($MATCH->{"Perlito5::Expression.list_parse"});
            if ($list eq '*undef*') {
                $list = undef
            }
            else {
                my $m = $MATCH->{"Perlito5::Expression.list_parse"};
                my $list_code = substr( $str, $m->{from}, $m->{to} - $m->{from} );

                # TODO - set the lexical context for eval

                my @list = eval $list_code;  # this must be evaluated in list context
                $list = \@list;
            }

            my $ast = Perlito5::AST::Use->new(
                    code      => Perlito5::Match::flat($MATCH->{use_decl}),
                    mod       => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.full_ident"}),
                    arguments => $list
                );

            parse_time_eval($ast);

            $MATCH->{capture} = [ 'term', $ast ];
        }
    ]
};

sub parse_time_eval {
    my $self = shift;

    my $module_name = $self->mod;
    my $use_or_not  = $self->code;
    my $arguments   = $self->{arguments};

    # test for "empty list" (and don't call import)
    my $skip_import = defined($arguments) && @$arguments == 0;

    $arguments = [] unless defined $arguments;

    if (
       $module_name eq 'feature'
       )
    {
        # not implemented
    }
    else {

        if ( $Perlito5::EXPAND_USE ) {
            # normal "use" is not disabled, go for it

            $module_name = $Perlito_internal_module{$module_name}
                if exists $Perlito_internal_module{$module_name};

            # "require" the module
            my $filename = modulename_to_filename($module_name);
            # warn "# require $filename\n";
            require $filename;

            if (!$skip_import) {

                # TODO - temporarily set caller() to the current module under compilation

                # call import/unimport
                if ($use_or_not eq 'use') {
                    if (defined &{$module_name . '::import'}) {
                        $module_name->import(@$arguments);
                    }
                }
                elsif ($use_or_not eq 'no') {
                    if (defined &{$module_name . '::unimport'}) {
                        $module_name->unimport(@$arguments);
                    }
                }
            }

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
    $s =~ s{::}{/}g;
    return $s . '.pm';
}

sub filename_lookup {
    my $filename = shift;

    if ( exists $INC{$filename} ) {
        return "done" if $INC{$filename};
        die "Compilation failed in require";
    }

    for my $prefix (@INC, '.') {
        my $realfilename = "$prefix/$filename";
        if (-f $realfilename) {
            $INC{$filename} = $realfilename;
            return "todo";
        }
    }
    die "Can't locate $filename in \@INC ".'(@INC contains '.join(" ",@INC).').';
}

sub expand_use {
    my $comp_units = shift;
    my $stmt = shift;

    my $module_name = $stmt->mod;
    return
        if $module_name eq 'strict'
        || $module_name eq 'warnings'
        || $module_name eq 'feature';

    $module_name = $Perlito_internal_module{$module_name}
        if exists $Perlito_internal_module{$module_name};

    my $filename = modulename_to_filename($module_name);

    return 
        if filename_lookup($filename) eq "done";

    # say "  now use: ", $module_name;
     
    # TODO - look for a precompiled version

    my $realfilename = $INC{$filename};

    # warn "// now loading: ", $realfilename;
    # load source
    my $source = Perlito5::IO::slurp( $realfilename );

    # compile; push AST into comp_units
    # warn $source;
    my $m = Perlito5::Grammar->exp_stmts($source, 0);
    die "Syntax Error near ", $m->{to}
        if $m->{to} != length($source);
    push @$comp_units, @{ add_comp_unit(
        [
            Perlito5::AST::CompUnit->new(
                name => 'main',
                body => Perlito5::Match::flat($m),
            )
        ]
    ) };
}

sub add_comp_unit {
    my $parse = shift;
    my $comp_units = [];

    for my $comp_unit (@$parse) {
        if (defined $comp_unit) {
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
    }
    return $comp_units;
}

sub require {
    my $filename = shift;
    my $is_bareword = shift;

    $filename = modulename_to_filename($filename)
        if $is_bareword;

    return 
        if filename_lookup($filename) eq "done";

    $result = do $INC{$filename};

    if ($@) {
        $INC{$filename} = undef;
        die $@;
    } elsif (!$result) {
        delete $INC{$filename};
        warn $@ if $@;
        die "$filename did not return true value";
    } else {
        return $result;
    }
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

