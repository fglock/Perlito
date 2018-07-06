
package Perlito5::Grammar::Statement;
use Perlito5::Macro;


my @Statement_chars;
my %Statement;

sub add_statement {
    my $name = shift;
    my $param = shift;
    $Statement{$name} = $param;
    unshift @Statement_chars, scalar(@Statement_chars) + 1
        while @Statement_chars < length($name);
}

token stmt_yadayada {
    '...' 
    {
        $MATCH->{capture} = Perlito5::AST::Apply->new(
            code      => 'die',
            namespace => '',
            arguments => [ Perlito5::AST::Buf->new( buf => 'Unimplemented' ) ],
        );
    }
};

#
# format =
# format NAME =
#
#   # comment
#
#   . OPTIONAL-SPACE
#
#   ONE-LINE_OF-TEXT
#   . OPTIONAL-SPACE
#
#   ONE-LINE_OF-TEXT
#   { LIST,
#   LIST   }   # comment
#
#   ONE-LINE_OF-TEXT
#   LIST
#
#
# format BLOCK =
# @<<< @<<<
# {foo=>"bar"} # this is a block, not a hash!
# .
#
#
token stmt_format {
    'format' <.Perlito5::Grammar::Space::ws> 
    [ <Perlito5::Grammar::full_ident>
    | { $MATCH->{'Perlito5::Grammar::full_ident'} = 'STDOUT' }
    ]
    {
        # inject a here-doc request - see Perlito5::Grammar::String
        my $placeholder = Perlito5::AST::Apply->new(
            code      => 'list:<.>',
            namespace => '',
            arguments => [
                # XXX - this comment was originally on Perlito5::Grammar::String
                # XXX - test 12 t/base/lex.t fails if we don't use this "double-pointer"   '
                Perlito5::AST::Apply->new(
                    code      => 'list:<.>',
                    namespace => '',
                    arguments => []
                  )
            ]
        );
        push @Perlito5::Grammar::String::Here_doc, [
            'single_quote',
            $placeholder->{arguments}[0]{arguments},
            '.',  # delimiter
        ];
        $MATCH->{capture} =
            Perlito5::AST::Apply->new(
                code      => 'p5:format',
                namespace => '',
                arguments => [
                    Perlito5::AST::Buf->new(
                        buf => Perlito5::Match::flat($MATCH->{'Perlito5::Grammar::full_ident'}),
                    ),
                    $placeholder,
                ]
            );
    }
    <.Perlito5::Grammar::Space::opt_ws>
    '=' 
    # TODO - make sure there are only spaces or comments until the end of the line
    <.Perlito5::Grammar::Space::ws>  # this will read the 'here-doc' we are expecting
};

token stmt_package {
    'package' <.Perlito5::Grammar::Space::ws> <Perlito5::Grammar::full_ident>

    [

        # package X 1.001 ...
        <.Perlito5::Grammar::Space::ws>
        <Perlito5::Grammar::Use::version_string>
        {   my $version = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Use::version_string"});
            $MATCH->{_version} = $version;
        }
        <.Perlito5::Grammar::Space::opt_ws>
    |
        <.Perlito5::Grammar::Space::opt_ws>
    ]

    [
        # package X { block }
        {
            # set the package name before parsing the block
            my $name = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::full_ident"});
            $MATCH->{_package} = $Perlito5::PKG_NAME;
            $Perlito5::PACKAGES->{$name} = 1;
            $Perlito5::PKG_NAME = $name;
        }
        <Perlito5::Grammar::block> 
        {
            my $namespace = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::full_ident"});
            my @statements = @{ $MATCH->{'Perlito5::Grammar::block'}{capture}{stmts} };

            if (@statements == 1) {
                # the Perl-to-Java compiler uses this syntax for "annotations":
                #   package Put { import => 'java.Put' };
                my $stmt = $statements[0];
                if ($stmt && ref($stmt) eq 'Perlito5::AST::Apply' && ( $stmt->{code} eq 'infix:<=>>' || $stmt->{code} eq 'list:<,>')) {
                    # - wrap the "list AST into a "hashref" AST
                    push @Perlito::ANNOTATION, [
                        $namespace,
                        Perlito5::AST::Apply->new(
                            arguments => [ $stmt ],
                            code => 'circumfix:<{ }>',
                        ),
                    ];
                }
            }

            if ($MATCH->{_version}) {
                unshift @statements,
                    Perlito5::AST::Apply->new(
                        'arguments' => [
                            Perlito5::AST::Var->new(
                                'name' => 'VERSION',
                                'namespace' => $namespace,
                                'sigil' => '$',
                            ),
                            $MATCH->{_version},
                        ],
                        'code' => 'infix:<=>',
                    );
            }
            $MATCH->{capture} = 
                Perlito5::AST::Block->new(
                    stmts => [
                        Perlito5::AST::Apply->new(
                            code      => 'package',
                            arguments => [], 
                            namespace => $namespace,
                        ),
                        @statements,
                    ]
                );
            $Perlito5::PKG_NAME = $MATCH->{_package};
        }
    |
        # old syntax - set the package name in the same lexical context
        {
            my $name = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::full_ident"});
            $Perlito5::PACKAGES->{$name} = 1;
            $Perlito5::PKG_NAME = $name;
            $MATCH->{capture} = 
                 Perlito5::AST::Apply->new(
                    code      => 'package',
                    arguments => [], 
                    namespace => $name
                 );
        }
    ]
};


sub exp_stmt {
    my $str = $_[0];
    my $pos = $_[1];
    my $tok = join( "", @{$str}[ $pos .. $pos + 15 ] );

    for my $len ( @Statement_chars ) {
        my $term = substr($tok, 0, $len);
        if (exists($Statement{$term})) {
            my $m = $Statement{$term}->($_[0], $pos);
            return $m if $m;
        }
    }
    return 0;
}


my @Modifier_chars = (7, 6, 5, 4, 3, 2);
my %Modifier = (
    'if'     => 1, 
    'unless' => 1,  
    'when'   => 1, 
    'for'    => 1, 
    'foreach'=> 1, 
    'while'  => 1, 
    'until'  => 1, 
    'given'  => 1,
);

sub statement_modifier {
    my $str = $_[0];
    my $pos = $_[1];
    my $tok = join( "", @{$str}[ $pos .. $pos + 15 ] );

    my $expression = $_[2]; 
    for my $len ( @Modifier_chars ) {
        my $term = substr($tok, 0, $len);
        if (exists($Modifier{$term})) {
            my $m = modifier($_[0], $pos + $len, $term, $expression);
            return $m if $m;
        }
    }
    return 0;
}

sub modifier {
    my $str = $_[0];
    my $pos = $_[1];

    my $modifier = $_[2];
    my $expression = $_[3]; 

    my $modifier_exp = Perlito5::Grammar::Expression::exp_parse($str, $pos);
    if (!$modifier_exp) {
        Perlito5::Compiler::error "Expected expression after '", Perlito5::Match::flat($modifier), "'";
    }
    # TODO - require a statement terminator

    if ($modifier eq 'if') {
        return {
            'str' => $str, 'from' => $pos, 'to' => $modifier_exp->{to},
            capture => Perlito5::AST::Apply->new(
                'arguments' => [
                    Perlito5::Match::flat($modifier_exp),
                    $expression,
                ],
                'code' => 'infix:<&&>',
                'namespace' => '',
            ),
        };
    }
    if ($modifier eq 'unless') {
        return {
            'str' => $str, 'from' => $pos, 'to' => $modifier_exp->{to},
            capture => Perlito5::AST::Apply->new(
                'arguments' => [
                    Perlito5::Match::flat($modifier_exp),
                    $expression,
                ],
                'code' => 'infix:<||>',
                'namespace' => '',
            ),
        };
    }
    if ($modifier eq 'when') {
        return {
            'str' => $str, 'from' => $pos, 'to' => $modifier_exp->{to},
            capture => Perlito5::AST::When->new(
                cond      => Perlito5::Match::flat($modifier_exp),
                body      => $expression,
            ),
        };
    }
    if ($modifier eq 'while') {
        my $stmt = Perlito5::AST::While->new(
                cond     => Perlito5::Match::flat($modifier_exp),
                body     => $expression,
            );
        my $out = Perlito5::Macro::while_file($stmt);
        $stmt = $out if $out;
        return {
            'str' => $str, 'from' => $pos, 'to' => $modifier_exp->{to},
            capture => $stmt,
        };
    }
    if ($modifier eq 'until') {
        my $stmt = Perlito5::AST::While->new(
                cond     => Perlito5::AST::Apply->new(
                                'arguments' => [ Perlito5::Match::flat($modifier_exp) ],
                                'code'      => 'prefix:<!>',
                                'namespace' => '',
                            ),
                body     => $expression,
            );
        return {
            'str' => $str, 'from' => $pos, 'to' => $modifier_exp->{to},
            capture => $stmt,
        };
    }
    if  (  $modifier eq 'for'
        || $modifier eq 'foreach'
        ) 
    {
        return {
            'str' => $str, 'from' => $pos, 'to' => $modifier_exp->{to},
            capture => Perlito5::AST::For->new(
                cond     => Perlito5::Match::flat($modifier_exp),
                body     => $expression,
                topic    => Perlito5::AST::Var::SCALAR_ARG(),
            ) 
        };
    }
    Perlito5::Compiler::error "Unexpected statement modifier '$modifier'";
}


sub statement_parse {
    ## my $str = $_[0];
    ## my $pos = $_[1];
    ## my $stmt = substr($str, $pos, 30);
    ## warn "statement_parse: [ $stmt ] in $Perlito5::PKG_NAME\n";

    ## local @Perlito5::SCOPE_STMT;

    my $m = statement_parse_inner(@_);

    return $m if !@Perlito5::SCOPE_STMT;
    Perlito5::Grammar::Scope::check_variable_declarations();
    return $m;
}

sub statement_parse_inner {
    my $str = $_[0];
    my $pos = $_[1];
    # say "# statement_parse input: ",$str," at ",$pos;

    # the rule for subroutines seems to be: 
    # named subs are statements,
    # anonymous subs are plain terms.

    my $res = exp_stmt($str, $pos);
    if ($res) {
        # looks like a statement
        if ( ref($res->{capture}) eq 'Perlito5::AST::Apply' && $res->{capture}{code} eq 'circumfix:<{ }>' ) {
            # doesn't really look like a statement - maybe it is a { hash } instead of { block }
        }
        else {
            return $res
        }
    }
    $res = Perlito5::Grammar::Expression::exp_parse($str, $pos);
    if (!$res) {
        # say "# not a statement or expression";
        return;
    }

    # did we just see a label?
    if (  $str->[$res->{to}] eq ':'
       && ref($res->{capture}) eq 'Perlito5::AST::Apply'
       && $res->{capture}{bareword}
       )
    {
        my $label = $res->{capture}{code};
        # say "label $label";
        my $ws   = Perlito5::Grammar::Space::opt_ws( $str, $res->{to} + 1 );
        my $stmt = statement_parse( $str, $ws->{to} );
        if ($stmt) {
            $stmt->{capture}{label} = $label;
            return $stmt;
        }
        $res->{to} = $ws->{to};
        $res->{capture} = Perlito5::AST::Apply->new(
                'arguments' => [],
                'code'      => 'undef',
                'namespace' => '',
                'label'     => $label,
            );
        return $res;
    }

    # say "# look for a statement modifier";
    my $modifier = statement_modifier($str, $res->{to}, Perlito5::Match::flat($res));

    my $p = $modifier ? $modifier->{to} : $res->{to};
    my $terminator = $str->[$p];
    if (   $terminator ne ';'
        && $terminator ne '}'
        && $terminator ne '' )
    {
        my $type = "Number or Bareword";
        $type = "Number" if $terminator ge '0' && $terminator le '9';
        $type = "String" if $terminator eq '"' || $terminator eq "'";
        $type = "Scalar" if $terminator eq '$';
        $type = "Array"  if $terminator eq '@';
        Perlito5::Compiler::error "$type found where operator expected";
    }

    if (!$modifier) {
        # TODO - require a statement terminator
        return $res;
    }
    return $modifier;
}


Perlito5::Grammar::Statement::add_statement( '...'      => \&stmt_yadayada );
Perlito5::Grammar::Statement::add_statement( 'package'  => \&stmt_package );
Perlito5::Grammar::Statement::add_statement( 'format'   => \&stmt_format );


1;

=begin

=head1 NAME

Perlito5::Grammar::Statement - Parser and AST generator for Perlito

=head1 SYNOPSIS

    statement_parse($str)

=head1 DESCRIPTION

This module parses source code for Perl 5 statements and generates Perlito5 AST.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team.

=head1 COPYRIGHT

Copyright 2010, 2011, 2012 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

