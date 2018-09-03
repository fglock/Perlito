use v5;
use Perlito5::AST;
use strict;

package Perlito5::AST::CompUnit;
{
    sub emit_compile_time {
        my $self = $_[0];
        return __PACKAGE__->new(
            %$self,
            body => [ map { defined($_) && $_->emit_compile_time() } @{$self->{body}} ],
        );
    }
}

package Perlito5::AST::Int;
{
    sub emit_compile_time { return $_[0] }
}

package Perlito5::AST::Num;
{
    sub emit_compile_time { return $_[0] }
}

package Perlito5::AST::Buf;
{
    sub emit_compile_time { return $_[0] }
}

package Perlito5::AST::Block;
{
    sub emit_compile_time {
        my $self = $_[0];
        return __PACKAGE__->new(
            %$self,
            stmts => [ map { defined($_) && $_->emit_compile_time() } @{$self->{stmts}} ],
            ( $self->{continue}
            ? ( continue => $self->{continue}->emit_compile_time() )
            : () ),
            # continue => [ map { defined($_) && $_->emit_compile_time() } @{$self->{continue}} ],
        );
    }
}

package Perlito5::AST::Index;
{
    sub emit_compile_time {
        my $self = $_[0];
        return __PACKAGE__->new(
            %$self,
            obj => $self->{obj}->emit_compile_time(),
            index_exp => $self->{index_exp}->emit_compile_time(),
        );
    }
}

package Perlito5::AST::Lookup;
{
    sub emit_compile_time {
        my $self = $_[0];
        return __PACKAGE__->new(
            %$self,
            obj => $self->{obj}->emit_compile_time(),
            index_exp => $self->{index_exp}->emit_compile_time(),
        );
    }
}

package Perlito5::AST::Var;
{
    sub emit_compile_time {
        my $self = $_[0];
        return $self;
    }
}

package Perlito5::AST::Call;
{
    sub emit_compile_time {
        my $self = $_[0]; 
        my $invocant = $self->{invocant}->emit_compile_time();
        my $arguments;
        if ( $self->{method} eq 'postcircumfix:<[ ]>' ) {
            $arguments = $self->{arguments}->emit_compile_time();
        }
        elsif ( $self->{method} eq 'postcircumfix:<{ }>' ) {
            $arguments = $self->{arguments}->emit_compile_time();
        }
        else {
            $arguments = [ map { $_->emit_compile_time() } @{$self->{arguments}} ];
        }
        my $meth = $self->{method};
        if ( ref($meth) eq 'Perlito5::AST::Var' ) {
            $meth = $meth->emit_compile_time();
        }
        return __PACKAGE__->new(
            %$self,
            method => $meth,
            invocant => $invocant,
            arguments => $arguments,
        );
    }
}

package Perlito5::AST::Apply;
{
    sub emit_compile_time_args {
        my $self = $_[0];
        return () if !$self->{arguments};
        return map { $_->emit_compile_time() } @{$self->{arguments}};
    }
    sub emit_compile_time {
        my $self = $_[0];   
        my $code;
        if (ref $self->{code}) {
            $code = $self->{code}->emit_compile_time();
        }
        else {
            $code = $self->{code};
        }
        my $arguments;
        if (ref $self->{arguments}) {
            $arguments = [ map { $_->emit_compile_time() } @{$self->{arguments}} ];
        }
        else {
            $arguments = $self->{arguments};
        }
        my $special_arg;
        if (ref $self->{special_arg}) {
            $special_arg = $self->{special_arg}->emit_compile_time();
        }
        else {
            $special_arg = $self->{special_arg};
        }

        # allow this code to generate subs:
        # $ perl perlito5.pl -Isrc5/lib -Ccompile_time -e ' BEGIN { for my $v ("a" .. "c") { *{$v} = sub { *{$v} = \123; return shift() . $v } } } '
        #
        # add the extra instrumentation code:
        #   *name = sub {...}
        # to:
        #   Perlito5::Grammar::Scope::compile_time_glob_set()
        if ($self->{code} eq 'infix:<=>') {
            # print STDERR "# assign in BEGIN block\n";
            my $arg = $self->{arguments}->[0];
            if (ref($arg) eq 'Perlito5::AST::Apply' && $arg->{code} eq 'prefix:<*>') {
                # print STDERR "# set GLOB in BEGIN block\n";
                return Perlito5::AST::Apply->new(
                    code => 'compile_time_glob_set',
                    namespace => 'Perlito5::Grammar::Scope',
                    arguments => [
                        $arg->{arguments}->[0]->emit_compile_time(),
                        $self->{arguments}[1]->emit_compile_time(),
                        Perlito5::AST::Buf->new( buf => $Perlito5::PKG_NAME ),
                    ]
                );
            }
            elsif (ref($arg) eq 'Perlito5::AST::Var' && $arg->{sigil} eq '*') {
                return Perlito5::AST::Apply->new(
                    code => 'compile_time_glob_set',
                    namespace => 'Perlito5::Grammar::Scope',
                    arguments => [
                        Perlito5::AST::Buf->new( buf => ($arg->{namespace} || $arg->{_namespace}) . '::' . $arg->{name} ),
                        $self->{arguments}[1]->emit_compile_time(),
                        Perlito5::AST::Buf->new( buf => $Perlito5::PKG_NAME ),
                    ]
                );
            }
        }

        if ($self->{code} eq 'eval') {
            my $args = $self->{arguments};
            if (@$args && !$args->[0]->isa( "Perlito5::AST::Block" )) {
                # eval-string inside BEGIN block
                ## TODO
                ## return Perlito5::AST::Apply->new(
                ##     %$self,
                ##     arguments => [
                ##         Perlito5::AST::Apply->new(
                ##             code => 'generate_eval_string',
                ##             namespace => 'Perlito5::CompileTime::Dumper',
                ##             arguments => [
                ##                 @$args,
                ##             ],
                ##         ),
                ##     ],
                ## );
                return $self;
            }
        }

        if ( $self->{code} eq 'require' && !$self->{namespace} ) {
            # use the compile-time "require" command inside BEGIN blocks
            return Perlito5::AST::Apply->new(
                %$self,
                namespace => 'Perlito5::Grammar::Use',
            );
        }

        return __PACKAGE__->new(
            %$self,
            code => $code,
            arguments => $arguments,
            ( $special_arg ? ( special_arg => $special_arg ) : () ),
        );
    }
}

package Perlito5::AST::If;
{
    sub emit_compile_time {
        my $self = $_[0]; 
        return __PACKAGE__->new(
            %$self,
            cond => $self->{cond}->emit_compile_time(),
            body => $self->{body}->emit_compile_time(),
            otherwise => $self->{otherwise}->emit_compile_time(),
        );
    }
}

package Perlito5::AST::When;
{
    sub emit_compile_time {
        my $self = $_[0];
        return __PACKAGE__->new(
            %$self,
            cond => $self->{cond}->emit_compile_time(),
            body => $self->{body}->emit_compile_time(),
        );
    }
}


package Perlito5::AST::While;
{
    sub emit_compile_time {
        my $self = $_[0];
        return __PACKAGE__->new(
            %$self,
            cond => $self->{cond}->emit_compile_time(),
            body => $self->{body}->emit_compile_time(),
        );
    }
}

package Perlito5::AST::For;
{
    sub emit_compile_time {
        my $self = $_[0];
        my $cond;
        if (ref($self->{cond}) eq 'ARRAY') {
            # C-style for
            $cond = [ map { defined($_) ? $_->emit_compile_time() : $_ } @{$self->{cond}} ];
        }
        else {
            $cond = $self->{cond}->emit_compile_time(),
        }
        return __PACKAGE__->new(
            %$self,
            cond => $cond,
            body => $self->{body}->emit_compile_time(),
            ( $self->{continue}
                ? ( continue => $self->{continue}->emit_compile_time() )
                : () ),
        );
    }
}

package Perlito5::AST::Decl;
{
    sub emit_compile_time {
        my $self = $_[0];
        return __PACKAGE__->new(
            %$self,
            var  => $self->{var}->emit_compile_time(),
        );
    }
}

package Perlito5::AST::Sub;
{
    sub emit_compile_time {
        my $self = $_[0];
        my @stmts;
        if (defined $self->{block}) {
            # this is not a pre-declaration

            if ($self->{name}) {
                # transform into anonymous sub
                local $Perlito5::PKG_NAME = $self->{namespace};
                return Perlito5::AST::Apply->new(
                    'code' => 'infix:<=>',
                    'namespace' => '',
                    'arguments' => [
                        Perlito5::AST::Var->new( 
                            'sigil' => '*',
                            '_decl' => 'global',
                            'namespace' => $self->{namespace},
                            'name' => $self->{name},
                        ),
                        Perlito5::AST::Sub->new(
                            %$self,
                            'namespace' => undef,
                            'name' => undef,
                        )->emit_compile_time(),
                    ],
                );
            }


            @stmts = @{$self->{block}{stmts}};
            @stmts = map { $_->emit_compile_time() } @stmts;

            {
                # at compile-time only:
                #   we are compiling - maybe inside a BEGIN block
                #   add extra instrumentation code
                #   provide a way to dump this closure

                # get list of captured variables, including inner blocks
                my @captured;
                for my $stmt (@{$self->{block}{stmts}}) {
                    push @captured, $stmt->get_captures();
                }
                my %dont_capture = map { $_->{dont} ? ( $_->{dont} => 1 ) : () } @captured;
                my %capture = map { $_->{dont} ? ()
                                  : $dont_capture{ $_->{_id} } ? ()
                                  : ($_->{_decl} eq 'local' || $_->{_decl} eq 'global' || $_->{_decl} eq '') ? ()
                                  : ( $_->{_id} => $_ )
                                  } @captured;

                # return a hash with { "variable name" => \"variable value" }
                # with all captured variables
                #   @_ && ref($_[0]) eq "Perlito5::dump" && return { '$x' => \$x }


                # save the unprocessed AST for this sub
                my $code = __PACKAGE__->new(
                    %$self,
                );
                my $id = Perlito5::get_label();
                $Perlito5::BEGIN_SUBS{$id} = $code;
                # warn "BEGIN_SUBS: ", Perlito5::Dumper::Dumper( $code );
                $Perlito5::BEGIN_LEXICALS{$_} = $capture{$_} for keys %capture;

                if (!@stmts) {
                    unshift @stmts,
                        Perlito5::AST::Apply->new(
                            code => 'return',
                            arguments => [],
                        );
                }

                unshift @stmts,
                  Perlito5::AST::Apply->new(
                    code => 'infix:<&&>',
                    arguments => [
                        Perlito5::AST::Var::LIST_ARG(),
                        Perlito5::AST::Apply->new(
                            code => 'infix:<&&>',
                            arguments => [
                                Perlito5::AST::Apply->new(
                                    code => 'infix:<eq>',
                                    arguments => [
                                        Perlito5::AST::Apply->new(
                                            'arguments' => [
                                                Perlito5::AST::Var::LIST_ARG_INDEX(0),
                                            ],
                                            'code' => 'ref',
                                        ),
                                        Perlito5::AST::Buf->new( 'buf' => 'Perlito5::dump' ),
                                    ],
                                ),
                                Perlito5::AST::Apply->new(
                                    code => 'return',
                                    arguments => [
                                        Perlito5::AST::Apply::->new(
                                            code => 'circumfix:<{ }>',
                                            arguments => [
                                                Perlito5::AST::Buf->new(buf => '__SUB__'),
                                                Perlito5::AST::Buf->new(buf => $id),
                                                Perlito5::AST::Buf->new(buf => '__PKG__'),
                                                Perlito5::AST::Buf->new(buf => $Perlito5::PKG_NAME),
                                                map {(
                                                    Perlito5::AST::Buf->new(
                                                        buf => $_,
                                                    ),
                                                    Perlito5::AST::Apply->new(
                                                        code => 'prefix:<\\>',
                                                        arguments => [
                                                            $capture{$_},
                                                        ], 
                                                    ),
                                                )} sort keys %capture
                                            ],
                                        ),
                                    ],
                                ),
                            ]
                        ),
                    ],
                );
            }

            $self = __PACKAGE__->new(
                %$self,
                ( $self->{block}
                ? ( block => Perlito5::AST::Block->new(
                            %{$self->{block}},
                            stmts => [ @stmts ],
                         ) )
                : () ),
            );
        }

        return $self;
    }
}

1;

=begin

=head1 NAME

Perlito5::AST::CompileTime - Code generator for Perlito5-in-CompileTime

=head1 SYNOPSIS

    $program->emit_compile_time()  # generated CompileTime code

=head1 DESCRIPTION

This module generates CompileTime code for the Perlito compiler.
CompileTime code is the code that runs in BEGIN blocks.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.

=head1 COPYRIGHT

Copyright 2016 by Flavio Soibelmann Glock.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
