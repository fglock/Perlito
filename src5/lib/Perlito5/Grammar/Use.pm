
package Perlito5::Grammar::Use;

use Perlito5::Grammar::Precedence;
use Perlito5::Grammar;
use strict;

my %Perlito_internal_module = (
    strict         => 'Perlito5X::strict',
    warnings       => 'Perlito5X::warnings',
    'warnings::register' => 'Perlito5X::warnings::register',
    feature        => 'Perlito5X::feature',
    utf8           => 'Perlito5X::utf8',
    bytes          => 'Perlito5X::bytes',
    re             => 'Perlito5X::re',
    encoding       => 'Perlito5X::encoding',
    Carp           => 'Perlito5X::Carp',
    Config         => 'Perlito5X::Config',
    Exporter       => 'Perlito5X::Exporter',
    'Data::Dumper' => 'Perlito5X::Dumper',
    UNIVERSAL      => 'Perlito5X::UNIVERSAL',
    IO             => 'Perlito5X::IO',
    JSON           => 'Perlito5X::JSON',
    Symbol         => 'Perlito5X::Symbol',
    'Tie::Hash'    => 'Perlito5X::Tie::Hash',
    'Tie::Array'   => 'Perlito5X::Tie::Array',
    'Tie::Scalar'  => 'Perlito5X::Tie::Scalar',
    # vars     => 'Perlito5::vars',         # this is "hardcoded" in stmt_use()
    # constant => 'Perlito5::constant',
);

sub register_internal_module {
    my ($module, $real_name) = @_;
    $Perlito_internal_module{$module} = $real_name;
}

token use_decl { 'use' | 'no' };

token version_string {
        <Perlito5::Grammar::Number::val_version>
        {   # "v5", "v5.8", "v5.8.1"
            $MATCH->{capture} = $MATCH->{"Perlito5::Grammar::Number::val_version"}{capture};
        }
    |   <Perlito5::Grammar::Number::term_digit>
        {   # "5", "5.8", "5.8.1", "5.008_001"
            my $version = $MATCH->{"Perlito5::Grammar::Number::term_digit"}{capture}[1]{buf}
                       || $MATCH->{"Perlito5::Grammar::Number::term_digit"}{capture}[1]{int}
                       || $MATCH->{"Perlito5::Grammar::Number::term_digit"}{capture}[1]{num};
            $MATCH->{capture} = Perlito5::AST::Buf->new( buf => $version );
        }
}; 

token term_require {
    # require BAREWORD
    'require' <.Perlito5::Grammar::Space::ws>
    [   <version_string>
        {   my $version = $MATCH->{"version_string"}{capture};
            $version->{is_version_string} = 1;  # AST annotation
            $MATCH->{capture} = [ 'term', Perlito5::AST::Apply->new(
                                   code      => 'require',
                                   namespace => '',
                                   arguments => [ $version ],
                                ) ];
        }
    |   <Perlito5::Grammar::full_ident>
        {   my $module_name = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::full_ident"});
            my $filename = modulename_to_filename($module_name);
            $MATCH->{capture} = [ 'term', Perlito5::AST::Apply->new(
                                   code      => 'require',
                                   namespace => '',
                                   arguments => [ Perlito5::AST::Buf->new( buf => $filename ) ],
                                ) ];
        }
    ]
};

token stmt_use {
    <use_decl> <.Perlito5::Grammar::Space::ws>
    [
        <version_string>
        {   # "use v5", "use v5.8" - check perl version
            my $version = $MATCH->{"version_string"}{capture}{buf};
            Perlito5::test_perl_version($version);
            $MATCH->{capture} = Perlito5::Grammar::Block::ast_nop();
        }
    |
        <Perlito5::Grammar::full_ident>  [ '-' <Perlito5::Grammar::ident> ]?
            [ <.Perlito5::Grammar::Space::ws> <version_string> <.Perlito5::Grammar::Space::opt_ws> <!before ',' > ]?
            [ <Perlito5::Grammar::Expression::exp_parse> | <.Perlito5::Grammar::Space::opt_ws> ]
        {
            # TODO - test the module version
            my $version = $MATCH->{"version_string"}[0]{capture}{buf};

            my $list = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression::exp_parse"});
            if (ref($list) eq 'Perlito5::AST::Buf') {
                # use feature 'say';
                $list = [ $list->{buf} ];
            }
            elsif ($list) {
                # evaluate the parameter list in a BEGIN-block context
                Perlito5::Grammar::Scope::check_variable_declarations();
                my $ast = Perlito5::AST::Block::->new(
                    'stmts' => [
                        Perlito5::AST::Apply->new(
                            code      => 'circumfix:<[ ]>',
                            arguments => [ $list ],
                        )
                    ]
                );
                $list = Perlito5::Grammar::Block::eval_begin_block($ast);
            }
            else {
                $list = undef;
            }

            my $full_ident = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::full_ident"});
            $Perlito5::PACKAGES->{$full_ident} = 1;

            my $use_decl = Perlito5::Match::flat($MATCH->{use_decl});

            if ($use_decl eq 'use' && $full_ident eq 'vars' && $list) {
                for my $name (@$list) {
                    #  $v --> $Pkg::v
                    $Perlito5::VARS{ substr($name,0,1) . $Perlito5::PKG_NAME . '::' . substr($name,1) } = 1;
                }
                # Perlito5::Compiler::error "not a valid variable name: @$list"
                #    if !$m;
                $MATCH->{capture} = Perlito5::Grammar::Block::ast_nop();
            }
            elsif ($use_decl eq 'use' && $full_ident eq 'constant') {
                my @ast;
                if ($list) {
                    my $name = shift @$list;
                    if (ref($name) eq 'HASH') {
                        for my $key (sort keys %$name ) {
                            my $code = 'sub ' . $key . ' () { ' 
                                .   Perlito5::Dumper::_dumper($name->{$key})
                                . ' }';
                            # say "will do: $code";
                            my $m = Perlito5::Grammar::Statement::statement_parse( [ split "", $code ], 0);
                            Perlito5::Compiler::error "not a valid constant: @$list"
                                if !$m;
                            # say Perlito5::Dumper::Dumper($m->{capture});
                            push @ast, $m->{capture};
                        }
                    }
                    else {
                        my $code = 'sub ' . $name . ' () { (' 
                            . join(', ', 
                                map { Perlito5::Dumper::_dumper($_) }
                                    @$list
                              )
                            . ') }';
                        # say "will do: $code";
                        my $m = Perlito5::Grammar::Statement::statement_parse( [ split "", $code ], 0);
                        Perlito5::Compiler::error "not a valid constant: @$list"
                            if !$m;
                        # say Perlito5::Dumper::Dumper($m->{capture});
                        push @ast, $m->{capture};
                    }
                }
                $MATCH->{capture} = Perlito5::AST::Block->new( stmts => \@ast );
            }
            else {
                if ($Perlito5::EMIT_USE) {
                    $MATCH->{capture} = Perlito5::AST::Apply->new(
                        code => 'use',
                        # namespace => 'CORE',
                        special_arg => 
                            Perlito5::AST::Apply->new(
                                code => $full_ident,
                                bareword => 1,
                                arguments => [],
                            ),
                        arguments => $list,
                    );
                }
                else {
                    $MATCH->{capture} = parse_time_eval(
                        {
                            mod       => $full_ident,
                            code      => $use_decl,
                            arguments => $list
                        }
                    );
                }
            }
        }
    |
        { Perlito5::Compiler::error "Syntax error" }
    ]
};

sub parse_time_eval {
    my $ast = shift;

    my $module_name = $ast->{mod};
    my $use_or_not  = $ast->{code};
    my $arguments   = $ast->{arguments};

    # test for "empty list" (and don't call import)
    my $skip_import = defined($arguments) && @$arguments == 0;

    $arguments = [] unless defined $arguments;

    # the first time the module is seen,
    # load the module source code and create a syntax tree.
    # the module runs in a new scope without access to the current lexical variables

    if ( $Perlito5::EXPAND_USE ) {
        # normal "use" is not disabled, go for it:
        #   - require the module (evaluate the source code)
        #   - run import()

        my $current_module_name = $Perlito5::PKG_NAME;

        # "require" the module
        my $filename = modulename_to_filename($module_name);
        # warn "# require $filename\n";
        # require $filename;
        #   use the compiler "require" instead of native Perl
        #   because we need to add BEGIN-block instrumentation
        Perlito5::Grammar::Use::require($filename);

        if (!$skip_import) {
            # call import/unimport
            if ($use_or_not eq 'use') {
                my $code = $module_name->can('import');
                if (defined($code)) {
                    # make sure that caller() points to the current module under compilation
                    unshift @Perlito5::CALLER, [ $current_module_name ];
                    eval {
                        # "package $current_module_name;\n"
                        $module_name->import(@$arguments);
                        1
                    }
                    or Perlito5::Compiler::error $@;
                    shift @Perlito5::CALLER;
                }
            }
            elsif ($use_or_not eq 'no') {
                my $code = $module_name->can('unimport');
                if (defined($code)) {
                    # make sure that caller() points to the current module under compilation
                    unshift @Perlito5::CALLER, [ $current_module_name ];
                    eval {
                        # "package $current_module_name;\n"
                        $module_name->unimport(@$arguments);
                        1
                    }
                    or Perlito5::Compiler::error $@;
                    shift @Perlito5::CALLER;
                }
            }
        }
    }
    else {
        # force "use" code to be inlined instead of eval-ed
        bootstrapping_use($ast);
    }

    return Perlito5::Grammar::Block::ast_nop();
}

sub emit_time_eval {
    my $ast = shift;

    if ($ast->{mod} eq 'strict') {
        if ($ast->{code} eq 'use') {
            strict->import();
        }
        elsif ($ast->{code} eq 'no') {
            strict->unimport();
        }
    }
}

sub modulename_to_filename {
    my $s = shift;
    $s = $Perlito_internal_module{$s}
        if exists $Perlito_internal_module{$s};
    $s =~ s{::}{/}g;
    return $s . '.pm';
}

sub filename_lookup {
    my $filename = shift;

    if ( exists $INC{$filename} ) {
        return "done" if $INC{$filename};
        Perlito5::Compiler::error "Compilation failed in require";
    }

    for my $prefix (@INC, '.') {
        my $realfilename = "$prefix/$filename";
        if (-f $realfilename) {
            $INC{$filename} = $realfilename;
            return "todo";
        }
    }
    Perlito5::Compiler::error "Can't locate $filename in \@INC ".'(@INC contains '.join(" ",@INC).').';
}

sub bootstrapping_use {
    # force "use" code to be inlined instead of eval-ed
    my $stmt = shift;

    my $module_name = $stmt->{mod};

    my $filename = modulename_to_filename($module_name);

    return 
        if filename_lookup($filename) eq "done";

    # say "  now use: ", $module_name;
     
    # TODO - look for a precompiled version

    local $Perlito5::FILE_NAME = $filename;
    local $Perlito5::LINE_NUMBER = 1;
    my $realfilename = $INC{$filename};

    # warn "// now loading: ", $realfilename;
    # load source
    open FILE, '<', $realfilename
      or Perlito5::Compiler::error "Cannot read $realfilename: $!\n";
    local $/ = undef;
    my $source = <FILE>;
    close FILE;

    # compile; push AST into comp_units
    # warn $source;
    my $m = Perlito5::Grammar::exp_stmts($source, 0);
    Perlito5::Compiler::error "Syntax Error near ", $m->{to}
        if $m->{to} != length($source);

    if ($m->{'to'} != length($source)) {
        my $pos = $m->{'to'} - 10;
        $pos = 0 if $pos < 0;
        print "* near: ", substr( $source, $pos, 20 ), "\n";
        print "* filename: $realfilename\n";
        Perlito5::Compiler::error 'Syntax Error';
    }

    push @Perlito5::COMP_UNIT,
      Perlito5::AST::CompUnit->new(
        name => 'main',
        body => Perlito5::Match::flat($m),
      );
    return;
}

sub require {
    my $filename = shift;

    my $m2 = version_string( [ split '', $filename ], 0);
    if ($m2) {
        # "use v5", "use v5.8" - check perl version
        my $version = $m2->{"version_string"}{capture}{buf};
        Perlito5::test_perl_version($version);
        return 1;
    }

    return 
        if filename_lookup($filename) eq "done";

    #   use the compiler "do FILE" instead of native Perl
    #   because we need to add BEGIN-block instrumentation
    # my $result = do $filename;
    my $source = slurp_file($filename);
    # print STDERR "require $filename [[ $source ]]\n";
    local $Perlito5::FILE_NAME = $filename;
    Perlito5::Grammar::Scope::check_variable_declarations();
    Perlito5::Grammar::Scope::create_new_compile_time_scope();
    local $^H = 0;
    local %^H = ();

    my $m = Perlito5::Grammar::exp_stmts($source, 0);
    my $ast = Perlito5::AST::Block->new( stmts => Perlito5::Match::flat($m) );
    # use Data::Dumper;
    # print STDERR Dumper $ast;
    my $result = Perlito5::Grammar::Block::eval_begin_block($ast);
    # print STDERR "result from require: ", Dumper $result;

    Perlito5::Grammar::Scope::end_compile_time_scope();

    if ($@) {
        $INC{$filename} = undef;
        Perlito5::Compiler::error $@;
    } elsif (!$result) {
        delete $INC{$filename};
        warn $@ if $@;
        Perlito5::Compiler::error "$filename did not return true value";
    } else {
        return $result;
    }
}

sub slurp_file {
    my $filename = shift;
    eval {
        filename_lookup($filename);
        1;
    } or do {
        $INC{$filename} = undef;
        $@ = '';
        $! = 'No such file or directory';
        return 'undef';
    };
    my $realfilename = $INC{$filename};
    open FILE, '<', $realfilename
      or Perlito5::Compiler::error "Cannot read $realfilename: $!\n";
    local $/ = undef;
    my $source = <FILE>;
    close FILE;
    return $source;
}

Perlito5::Grammar::Statement::add_statement( 'no'  => \&stmt_use );
Perlito5::Grammar::Statement::add_statement( 'use' => \&stmt_use );
Perlito5::Grammar::Precedence::add_term( 'require' => \&term_require );


1;

=begin

=head1 NAME

Perlito5::Grammar::Use - Parser and AST generator for Perlito

=head1 SYNOPSIS

    stmt_use($str)

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

