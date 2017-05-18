
package Perlito5::Grammar::Bareword;
use strict;


token the_object {
        <before '$'> <Perlito5::Grammar::Sigil::term_sigil>
            {
                $MATCH->{capture} = Perlito5::Match::flat($MATCH->{'Perlito5::Grammar::Sigil::term_sigil'})->[1];
            }
    |
        '{' <Perlito5::Grammar::Expression::curly_parse> '}'
            {
                $MATCH->{capture} = Perlito5::Match::flat($MATCH->{'Perlito5::Grammar::Expression::curly_parse'});
            }
    |
        <Perlito5::Grammar::Print::typeglob>
            {
                $MATCH->{capture} = Perlito5::Match::flat($MATCH->{'Perlito5::Grammar::Print::typeglob'});
            }
};

sub term_bareword {
    my $str = $_[0];
    my $pos = $_[1];

    my $p = $pos;
    my $m_namespace = Perlito5::Grammar::optional_namespace_before_ident( $str, $p );
    my $namespace = Perlito5::Match::flat($m_namespace);
    $p = $m_namespace->{to};
    my $m_name      = Perlito5::Grammar::ident( $str, $p );

    if (!$m_name) {
        if ($namespace) {
            # namespace without name - X::
            $m_namespace->{capture} = [ 'term', 
                        Perlito5::AST::Var->new(
                            sigil => '::',
                            name  => '',
                            namespace => $namespace,
                        )
                    ];
            return $m_namespace;
        }
        return;
    }

    my $name = Perlito5::Match::flat($m_name);
    if ($name eq '__PACKAGE__' && $namespace eq '') {
        $m_name->{capture} = [ 'term', Perlito5::AST::Apply->new( code => $name, namespace => '', arguments => [], bareword => 1 ) ];
        return $m_name;
    }
    $p = $m_name->{to};

    if ( $str->[$p] eq ':' && $str->[$p+1] eq ':' ) {
        # ::X::y::
        $m_name->{to} = $p + 2;
        $m_name->{capture} = [ 'term', 
                    Perlito5::AST::Var->new(
                        sigil => '::',
                        name  => '',
                        namespace => $namespace . '::' . $name,
                    )
                ];
        return $m_name;
    }

    my $full_name = $name;
    $full_name = $namespace . '::' . $name if $namespace;

    # we've got a bareword

    # my $has_space_after;
    my $m = Perlito5::Grammar::Space::ws( $str, $p );
    if ( $m ) {
        # $has_space_after = 1;
        $p = $m->{to};
    }

    if ( $str->[$p] eq '!' && ( $str->[$p+1] eq '=' || $str->[$p+1] eq '~' ) ) {
        # != or !~
        # "X::y != ..."  subroutine call
        $m_name->{capture} = [ 'term', 
                    Perlito5::AST::Apply->new(
                        code      => $name,
                        namespace => $namespace,
                        arguments => [],
                        bareword  => 1
                    )
                ];
        return $m_name;
    }

    # check for indirect-object
    my $invocant;
    my $is_subroutine_name;
    my $effective_name = ( $namespace || $Perlito5::PKG_NAME ) . '::' . $name;

    {
        # workaround for bootstrapping: $Perlito5::PROTO is not set when executing under the 'C-perl' compiler
        my $p = eval { prototype($effective_name) };
        $Perlito5::PROTO->{$effective_name} = $p if $p;
    }

    if ( exists( $Perlito5::Grammar::Print::Print{$name} ) ) {
        $invocant = undef;
    }
    elsif (  exists( $Perlito5::PROTO->{$effective_name} )       # subroutine was predeclared
           || ( (!$namespace || $namespace eq 'CORE')
               && exists $Perlito5::CORE_PROTO->{"CORE::$name"}  # subroutine comes from CORE
             )
           )
    {
        # first term is a subroutine name;
        $is_subroutine_name = 1;

        # this can be an indirect-object if the next term is a bareword ending with '::'

        $invocant = Perlito5::Grammar::full_ident( $str, $p );
        if ($invocant) {
            my $package = Perlito5::Match::flat($invocant);
            if ( $package ) {
                $invocant->{capture} = Perlito5::AST::Var->new(
                                         sigil => '::',
                                         name  => '',
                                         namespace => $package,
                                     );
                if ( $str->[$invocant->{to}] eq ':' && $str->[$invocant->{to}+1] eq ':' ) {
                    # ::X::y::
                    $invocant->{to} = $invocant->{to} + 2;
                }
                else {
                    # is this a known package name?
                    if ( ! $Perlito5::PACKAGES->{ $package } ) {
                        # not a known package name
                        $invocant = undef;
                    }
                    else {
                        # valid package name - this is indirect-object
                    }
                }
            }
        }

    }
    else {
        $invocant = Perlito5::Grammar::Bareword::the_object( $str, $p );
    }

    if ($invocant) {
        # indirect-object
        $p = $invocant->{to};

        # read the parameter list
        my $arg = [];
        $m = Perlito5::Grammar::Space::ws( $str, $p );
        $p = $m->{to} if $m;
        if ( $str->[$p] eq '-' && $str->[$p+1] eq '>' ) {
            # ->
        }
        elsif ( $str->[$p] eq '(' ) {
            my $m = Perlito5::Grammar::Expression::term_paren( $str, $p );
            if ( $m ) {
                $arg = $m->{capture}[2];
                $p   = $m->{to};
                $arg = Perlito5::Grammar::Expression::expand_list( $arg );
            }
        }
        else {
            my $m = Perlito5::Grammar::Expression::list_parse( $str, $p );
            if ($m->{capture} ne '*undef*') {
                $arg = Perlito5::Grammar::Expression::expand_list( $m->{capture} );
                $p   = $m->{to};
            }
        }
        $m_name->{capture} = [ 
            'term', 
            Perlito5::AST::Call->new(
                'method'    => $full_name,
                'invocant'  => Perlito5::Match::flat($invocant), 
                'arguments' => $arg,
            ),
        ];
        $m_name->{to} = $p;
        return $m_name;
    }

    if ( $str->[$p] eq '=' && $str->[$p+1] eq '>' ) {
        # =>
        # autoquote bareword
        $m_name->{capture} = [ 'term', 
                    Perlito5::AST::Apply->new(
                        code      => $name,
                        namespace => $namespace,
                        arguments => [],
                        bareword  => 1
                    )
                ];
        $m_name->{to} = $p;
        return $m_name;
    }
    if ( $str->[$p] eq '-' && $str->[$p+1] eq '>' ) {
        # ->
        if ( $is_subroutine_name ) {
            # call()->method call
            $m_name->{capture} = [ 'term', 
                                   Perlito5::AST::Apply->new(
                                       'arguments' => [],
                                       'code'      => $name,
                                       'namespace' => $namespace,
                                   )
                                 ];
        }
        else {
            # class->method call
            $m_name->{capture} = [ 'term',
                                   Perlito5::AST::Var->new(
                                        'name'      => '',
                                        'namespace' => $full_name,
                                        'sigil'     => '::',
                                   )
                                 ];
        }
        $m_name->{to} = $p;
        return $m_name;
    }

    # Note: how does this work: (See: perldoc CORE)
    #   $ perl -e ' use strict; sub print { die "here" }; print "123\n"; '
    #   123
    #   $ perl -e ' use strict; BEGIN { *CORE::GLOBAL::time = sub { die "here" } }; print time . "\n"; '
    #   here at -e line 1.
    #   $ perl -e ' use strict; BEGIN { *CORE::GLOBAL::print = sub { die "here" } }; print "123\n"; '
    #   123
    #   $ perl -e ' use strict; use subs "print"; sub print { die "here" }; print "123\n"; '
    #   123
    #
    #   * if it has a prototype it's overridable (thanks rjbs++ in #p5p)
    #   * however, 'chomp' => undef, but I can override it
    #   * core functions that have undefined prototypes and that are overridable:
    #   chomp chop glob exec system require chomp glob exec glob system
    #
    #   $ perl -e ' use strict; BEGIN { *CORE::GLOBAL::chomp = sub { die "here" } }; print chomp . "\n"; '
    #   here at -e line 1.
    
    #   $ perl -e ' use strict; use Data::Dumper; print Dumper prototype("CORE::time")  '
    #   $VAR1 = '';  -> can be overridden
    #   $ perl -e ' use strict; use Data::Dumper; print Dumper prototype("CORE::print")  '
    #   $VAR1 = undef;  -> can't be overwritten

    #   * list of prototypes in CORE:
    #
    #   $ perldoc -u PerlFunc | head -n300 | perl -ne ' push @x, /C<([^>]+)/g; END { eval { $p{$_} = prototype("CORE::$_") } for @x; use Data::Dumper; print Dumper \%p } '


    my $sig;
    if ( exists $Perlito5::PROTO->{$effective_name} ) {
        # subroutine was predeclared
        $sig = $Perlito5::PROTO->{$effective_name};
    }
    elsif ( (!$namespace || $namespace eq 'CORE')
          && exists $Perlito5::CORE_PROTO->{"CORE::$name"} 
          )
    {
        # TODO - CORE::GLOBAL

        # subroutine comes from CORE

        ## XXX - this breaks perl: "CORE::say is not a keyword"
        ## $namespace = "CORE";

        $effective_name = "CORE::$name";
        $sig = $Perlito5::CORE_PROTO->{$effective_name};
    }
    else {
        # TODO - add error messages if needed
        # Bareword "X" not allowed while "strict subs" in use

        # warn "not found: $effective_name";

        ## # it's just a bareword - we will disambiguate later
        ## $m_name->{capture} = [ 'postfix_or_term', 'funcall_no_params',
        ##         $namespace,
        ##         $name
        ##     ];
        ## return $m_name;

        # check for vstring: v100.200.300
        my $m = Perlito5::Grammar::Number::val_version( $str, $pos );
        if ($m) {
            $m->{capture} = [ 'term', $m->{capture} ];
            return $m;
        }

        $sig = undef;
    }

    # TODO - parse the parameter list according to the sig

    # say "calling $effective_name ($sig)";

    my $has_paren = 0;
    if ( defined $sig ) {
        my $arg_index = 1;
        my $optional = 0;
        my @args;

        my $sig_part = substr($sig, 0, 1);
        my $m;
        my $capture;

        if ($sig_part eq '&') {
            $m = Perlito5::Grammar::Space::ws( $str, $p );
            $p = $m->{to} if $m;
            if ( $str->[$p] ne '(' ) {
                $sig = substr($sig, 1);
                $m = Perlito5::Grammar::Bareword::prototype_is_ampersand( $str, $p );
                $capture = $m->{capture} if $m;
                if (!$m) {
                    Perlito5::Compiler::error("Type of arg $arg_index to $name must be block or sub {}");
                }
                $p = $m->{to};
                push @args, $capture;
                # there is no comma after first '&'
                # but '(' is allowed here
            }
        }

        if ( substr($sig, 0, 1) eq ';' && $str->[$p] eq '/' && $str->[$p+1] eq '/' ) {
            # argument is optional - shift(), pop() followed by //
            #
            # special case - see test t5/01-perlito/25-syntax-defined-or.t
            # we don't want '//' to be an argument (match) but an operator (defined-or)
            # so we return 'bareword'
            $m_name->{capture} = [ 'term', 
                    Perlito5::AST::Apply->new(
                        code      => $name,
                        namespace => $namespace,
                        arguments => [],
                        bareword  => 1,
                    )
                ];
            $m_name->{to} = $p;
            return $m_name;
        }

        if ( $sig eq '' ) {
            # empty sig - we allow (), but only if it is empty
            if ( $str->[$p] eq '(' ) {
                $p++;
                $has_paren = 1;
                my $m = Perlito5::Grammar::Space::ws( $str, $p );
                if ($m) {
                    $p = $m->{to}
                }
                if ( $str->[$p] ne ')' ) {
                    Perlito5::Compiler::error( "syntax error near ", join("", @{$str}[ $pos .. $pos + 10 ] ));
                }
                $p++;
            }
            if ($name eq '__FILE__') {
                $m_name->{capture} = [ 'term', 
                    Perlito5::AST::Buf->new(
                        buf => $Perlito5::FILE_NAME,
                    )
                ];
            }
            elsif ($name eq '__LINE__') {
                $m_name->{capture} = [ 'term', 
                    Perlito5::AST::Int->new(
                        int => $Perlito5::LINE_NUMBER,
                    )
                ];
            }
            else {
                # TODO - "subs with empty protos are candidates for inlining"
                $m_name->{capture} = [ 'term', 
                    Perlito5::AST::Apply->new(
                        code      => $name,
                        namespace => $namespace,
                        arguments => \@args,
                        bareword  => ($has_paren == 0)
                    )
                ];
            }
            $m_name->{to} = $p;
            return $m_name;
        }

        ### SIG:
        ### while ($sig) {
        ###     # TODO: 'code' => 'circumfix:<( )>',
        ###     # TODO: comma
        ###     $m = Perlito5::Grammar::Space::ws( $str, $p );
        ###     $p = $m->{to} if $m;
        ###     $m = Perlito5::Grammar::Bareword::prototype_is_dollar( $str, $p );
        ###     $capture = $m->{capture} if $m;
        ###     if ($sig_part eq '_') {
        ###         if (!$m) {
        ###             $capture = Perlito5::AST::Var->new(
        ###                     namespace => '',
        ###                     name      => '_',
        ###                     sigil     => '$'
        ###                 );
        ###         }
        ###         else {
        ###             $p = $m->{to};
        ###         }
        ###         print Perlito5::Dumper::Dumper($capture);
        ###     }
        ###     elsif ($sig_part eq '$') {
        ###         if (!$m) {
        ###             last SIG if $optional;
        ###             Perlito5::Compiler::error "Not enough arguments for $name";
        ###         }
        ###         $p = $m->{to};
        ###         push @args, $capture;
        ###     }
        ###     elsif ($sig_part eq '&') {
        ###         if (!$m) {
        ###             last SIG if $optional;
        ###             Perlito5::Compiler::error "Type of arg $arg_index to $name must be sub {}";
        ###         }
        ###         $p = $m->{to};
        ###         push @args, $capture;
        ###     }
        ###     elsif ($sig_part eq '*') {
        ###         # TODO
        ###         #   stat FILEHANDLE
        ###         #   stat EXPR
        ###         #   stat DIRHANDLE
        ###         #   If EXPR is omitted, it stats $_.
        ###         Perlito5::Compiler::error "TODO: '*' sig";
        ###         push @args, $capture;
        ###     }
        ###     elsif ($sig_part eq ';') {
        ###         $optional = 1;
        ###         next SIG;
        ###     }
        ###     else {
        ###         Perlito5::Compiler::error "don't know what to do with sig '$sig_part'";
        ###     }
        ### }
        ### continue {
        ###     $sig_part = substr($sig, 0, 1);
        ###     $sig = substr($sig, 1);
        ### }
        ### print Perlito5::Dumper::Dumper(\@args);
        ### Perlito5::Compiler::error "TODO";

        if ( $sig eq '_' || $sig eq '$' || $sig eq '+' || $sig eq ';$' ) {
            my $m;
            my $arg;
            if ( $str->[$p] eq '(' ) {
                $m = Perlito5::Grammar::Expression::term_paren( $str, $p );
                if ( !$m ) { return $m };
                $p = $m->{to};
                $has_paren = 1;
                $arg = $m->{capture}[2];
                $arg = Perlito5::Grammar::Expression::expand_list( $arg );
                my $v = shift @{ $arg };
                Perlito5::Compiler::error( "Too many arguments for $name")
                    if @{ $arg };
                $arg = $v;
            }
            else {
                $m = Perlito5::Grammar::Expression::argument_parse( $str, $p );
                $arg = $m->{capture};
                if ( $arg eq '*undef*' ) {
                    $arg = undef;
                }
                elsif ( ref($arg) eq 'Perlito5::AST::Apply' && $arg->{code} eq 'circumfix:<( )>' ) {
                    my $v = shift @{ $arg->{arguments} };
                    Perlito5::Compiler::error( "Too many arguments for $name" )
                        if @{ $arg->{arguments} };
                    $arg = $v;
                }
            }
            if ( defined $arg ) {
                push @args, $arg;
                $has_paren = 1;
            }
            else {
                Perlito5::Compiler::error( "Not enough arguments for $name")
                    if $sig eq '$';
                push @args, Perlito5::AST::Var->new(
                            namespace => '',
                            name      => '_',
                            sigil     => '$'
                        )
                    if $sig eq '_';
                # ';$' --> ignore the missing arg
            }
            my $ast = Perlito5::AST::Apply->new(
                        code      => $name,
                        namespace => $namespace,
                        arguments => \@args,
                        bareword  => ($has_paren == 0)
                    );
            if ($name eq 'eval' && !$namespace) {
                # add scope information to eval-string
                # - here we add the variables declared so far in the current closure
                # - other variables captured by the current closure need to be added when the closure finishes compiling

                $ast->{_scope} = Perlito5::Grammar::Scope::get_snapshot( $Perlito5::CLOSURE_SCOPE );
            }
            $m->{capture} = [ 'term', $ast ];
            return $m;
        }

        if ( $sig eq ';@' || $sig eq '@' ) {
            if ( $str->[$p] eq '(' ) {
                $m = Perlito5::Grammar::Expression::term_paren( $str, $p );
                $has_paren = 1;
                my $arg = $m->{capture}[2];
                $arg = Perlito5::Grammar::Expression::expand_list( $arg );
                push @args, @$arg;
            }
            else {
                $m = Perlito5::Grammar::Expression::list_parse( $str, $p );
                my $arg = $m->{capture};
                if ($arg ne '*undef*') {
                    $arg = Perlito5::Grammar::Expression::expand_list( $arg );
                    push @args, @$arg;
                }
            }
            my $ast = Perlito5::AST::Apply->new(
                        code      => $name,
                        namespace => $namespace,
                        arguments => \@args,
                        bareword  => ($has_paren == 0)
                    );
            $m->{capture} = [ 'term', $ast ];
            return $m;
        }

        if ( $sig eq '*' ) {
            # TODO

            # stat FILEHANDLE
            # stat EXPR
            # stat DIRHANDLE
            # If EXPR is omitted, it stats $_.

            # $m = Perlito5::Grammar::Print::typeglob( $str, $p );
            # if ($m) {
            #     $p = $m->{to};
            #     my $arg = Perlito5::Match::flat($m->{'Perlito5::Grammar::Print::typeglob'});
            #     # TODO
            # }
        }

    } # / defined $sig


    # no sig


    # maybe it's a subroutine call

    if ( $str->[$p] eq '(' ) {
        $m = Perlito5::Grammar::Expression::term_paren( $str, $p );
        if ( !$m ) { return $m };
        my $arg = $m->{capture}[2];
        $arg = Perlito5::Grammar::Expression::expand_list( $arg );

        if ( $namespace eq '' || $namespace eq 'CORE' ) {
            if  (   $name eq 'local'
                ||  $name eq 'my'
                ||  $name eq 'state'
                ||  $name eq 'our'
                )
            {
                my $declarator = $name;
                for my $var (@$arg) {
                    if ( ref($var) eq 'Perlito5::AST::Apply' && $var->{code} eq 'undef' ) {
                        # "local (undef)" is a no-op
                    }
                    else {
                        my $decl = Perlito5::AST::Decl->new(
                                decl => $declarator,
                                type => '',
                                var  => $var,
                                attributes => [],
                            );
                        $var->{_decl} = $name;
                        $var->{_id}   = $Perlito5::ID++;
                        $var->{_namespace} = $Perlito5::PKG_NAME if $declarator eq 'our';
                        $var->{_namespace} = $Perlito5::PKG_NAME
                            if $declarator eq 'local' && !$var->{namespace} && !$var->{_namespace};
                    }
                }
            }
            if ( $name eq 'print' || $name eq 'say' ) {
                if (@$arg == 0) {
                    push @$arg, Perlito5::AST::Var->new(
                                                namespace => '',
                                                name      => '_',
                                                sigil     => '$'
                                            );
                }
            }
            if ( $name eq 'split' ) {
                if (@$arg == 0) {
                    push @$arg, Perlito5::AST::Buf->new( buf => ' ' );
                }
                if (@$arg == 1) {
                    push @$arg, Perlito5::AST::Var->new(
                                                namespace => '',
                                                name      => '_',
                                                sigil     => '$'
                                            );
                }
            }
        }

        $m->{capture} = [ 'term', 
                Perlito5::AST::Apply->new(
                    code      => $name,
                    namespace => $namespace,
                    arguments => $arg,
                    proto     => $sig,     # remember the proto at the time this was parsed
                )
            ];
        return $m;
    }


    my $m_list = Perlito5::Grammar::Expression::list_parse( $str, $p );
    my $list = $m_list->{capture};
    if ($list ne '*undef*') {
        $m_name->{capture} = [ 'postfix_or_term', 'funcall',
                $namespace,
                $name,
                $list
            ];
        $m_name->{to} = $m_list->{to};
        return $m_name;
    }

    if ( $namespace eq '' || $namespace eq 'CORE' ) {
        if ( $name eq 'print' || $name eq 'say' ) {
            $m_name->{capture} = [ 'term', 
                    Perlito5::AST::Apply->new(
                        code      => $name,
                        namespace => $namespace,
                        arguments => [ Perlito5::AST::Var->new(
                                            namespace => '',
                                            name      => '_',
                                            sigil     => '$'
                                        ),
                                     ],
                    )
                ];
            return $m_name;
        }
        if ( $name eq 'split' && ($namespace eq '' || $namespace eq 'CORE') ) {
            $m_name->{capture} = [ 'term', 
                    Perlito5::AST::Apply->new(
                        code      => $name,
                        namespace => $namespace,
                        arguments => [ 
                                       Perlito5::AST::Buf->new( buf => ' ' ),
                                       Perlito5::AST::Var->new(
                                            namespace => '',
                                            name      => '_',
                                            sigil     => '$'
                                        ),
                                     ],
                    )
                ];
            return $m_name;
        }
    }

    # it's just a bareword - we will disambiguate later

    if ($Perlito5::STRICT) {
        # Allow:
        #   - close FILE
        #   - LABEL: { ... }
        #   - next LABEL / goto LABEL
        #   - predeclared named subroutines in $Perlito5::PROTO
        #   - CORE operators                in $Perlito5::CORE_PROTO
        #   - subr LABEL                    if prototype(&subr) eq '*'

        my $m = Perlito5::Grammar::Space::opt_ws( $str, $p );
        my $p = $m->{to};
        if ( $str->[$p] eq ':' ) {
            # looks like "LABEL:"
        }
        elsif (
            !(
                exists( $Perlito5::PROTO->{$effective_name} )
                || (
                    ( !$namespace || $namespace eq 'CORE' )
                    && exists $Perlito5::CORE_PROTO->{"CORE::$name"}    # subroutine comes from CORE
                )
            )
          )
        {
            # TODO

            # subroutine was not predeclared
            # print STDERR "effective_name [$effective_name]\n";
            # Perlito5::Compiler::error( 'HERE2 Bareword "' . ( $namespace ? "${namespace}::" : "" ) . $name . '" not allowed while "strict subs" in use' );
            # warn( 'HERE2 Bareword "' . ( $namespace ? "${namespace}::" : "" ) . $name . '" not allowed while "strict subs" in use' );

        }
    }

    $m_name->{capture} = [ 'postfix_or_term', 'funcall_no_params',
            $namespace,
            $name
        ];
    return $m_name;
}

### sub prototype_is_dollar {
###     my $m = Perlito5::Grammar::Expression::argument_parse(@_);
###     return if $m->{capture} eq '*undef*';
###     return $m;
### }

token prototype_is_ampersand {
    # prototype is '&'
    |   'sub' <.Perlito5::Grammar::Space::opt_ws> <Perlito5::Grammar::Block::anon_sub_def>
            #  sub{block}  sub(proto){block}
            { $MATCH->{capture} = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Block::anon_sub_def"}) }
    |   <Perlito5::Grammar::block>
            #  {block}
            { $MATCH->{capture} = 
                Perlito5::AST::Sub->new(
                   'attributes' => [],
                   'block'      => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::block"}),
                   'name'       => undef,
                   'namespace'  => undef,
                   'sig'        => undef,
                );
            }
    |   <before '\\' <.Perlito5::Grammar::Space::opt_ws> '&' >
            #  \&sub
            <Perlito5::Grammar::Expression::argument_parse>
            { $MATCH->{capture} = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression::argument_parse"}) }
};

1;

=begin

=head1 NAME

Perlito5::Grammar::Bareword - Parser and AST generator for Perlito

=head1 SYNOPSIS

    term_bareword($str)

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

