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
    sub emit_compile_time { return @_ }
}

package Perlito5::AST::Num;
{
    sub emit_compile_time { return @_ }
}

package Perlito5::AST::Buf;
{
    sub emit_compile_time { return @_ }
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

        # TODO - uncomment this to allow this code to generate subs:
        # $ perl perlito5.pl -Isrc5/lib -Ccompile_time -e ' BEGIN { for my $v ("a" .. "c") { *{$v} = sub { *{$v} = \123; return shift() . $v } } } '
        #
        # remove the extra instrumentation code:
        #   Perlito5::Grammar::Scope::compile_time_glob_set()
        # back to:
        #   *name = sub {...}
        if ($self->{namespace} && $self->{namespace} eq 'Perlito5::Grammar::Scope'
            && $self->{code} eq 'compile_time_glob_set' )
        {
            $self = Perlito5::AST::Apply->new(
                code => 'infix:<=>',
                arguments => [
                    Perlito5::AST::Apply->new(
                        code => 'prefix:<*>',
                        arguments => [ $self->{arguments}[0] ],
                    ),
                    $self->{arguments}[1],
                ],
            );
        }
        # add the extra instrumentation code:
        #   *name = sub {...}
        # to:
        #   Perlito5::Grammar::Scope::compile_time_glob_set()
        if ($self->{code} eq 'infix:<=>' && $Perlito5::PHASE eq 'BEGIN') {
            # print STDERR "# assign in BEGIN block\n";
            my $arg = $self->{arguments}->[0];
            if (ref($arg) eq 'Perlito5::AST::Apply' && $arg->{code} eq 'prefix:<*>') {
                # print STDERR "# set GLOB in BEGIN block\n";
                return [ apply => '(', 'Perlito5::Grammar::Scope::compile_time_glob_set',
                                $arg->{arguments}->[0]->emit_compile_time(),
                                $self->{arguments}[1]->emit_compile_time(),
                                "'$Perlito5::PKG_NAME'" 
                       ];
            }
        }

        return __PACKAGE__->new(
            %$self,
            code => $code,
            arguments => $arguments,
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
            $cond = [ map { $_->emit_compile_time() } @{$self->{cond}} ];
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

            @stmts = @{$self->{block}{stmts}};

            # remove the extra instrumentation code:
            #   @_ && ref($_[0]) eq 'Perlito5::dump' && return ...
            if (@stmts) {
                my $stmt = $stmts[0];
                if (ref($stmt) eq 'Perlito5::AST::Apply' && $stmt->{'code'} eq 'infix:<&&>') {
                    $stmt = $stmt->{arguments}[1];
                    if (ref($stmt) eq 'Perlito5::AST::Apply' && $stmt->{'code'} eq 'infix:<&&>') {
                        $stmt = $stmt->{arguments}[0];
                        if (ref($stmt) eq 'Perlito5::AST::Apply' && $stmt->{'code'} eq 'infix:<eq>') {
                            $stmt = $stmt->{arguments}[1];
                            if (ref($stmt) eq 'Perlito5::AST::Buf' && $stmt->{'buf'} eq 'Perlito5::dump') {
                                shift @stmts;
                            }
                        }
                    }
                }
            }

            @stmts = map { $_->emit_compile_time() } @stmts;

            if ($Perlito5::PHASE eq 'BEGIN') {
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
                my @captures_ast  = values %capture;

                # return a hash with { "variable name" => \"variable value" }
                # with all captured variables
                #   @_ && ref($_[0]) eq "Perlito5::dump" && return { '$x' => \$x }


                # save the unprocessed AST for this sub
                my $code = __PACKAGE__->new(
                    %$self,
                    block => Perlito5::AST::Block->new(
                                %{$self->{block}},
                                stmts => [ @stmts ],
                             ),
                );
                my $id = Perlito5::get_label();
                $Perlito5::BEGIN_SUBS{$id} = $code;

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
                                                map {(
                                                    Perlito5::AST::Buf->new(
                                                        buf => ($_->{_real_sigil} || $_->{sigil}) . $_->{name},
                                                    ),
                                                    Perlito5::AST::Apply->new(
                                                        code => 'prefix:<\\>',
                                                        arguments => [
                                                            $_,
                                                        ], 
                                                    ),
                                                )} @captures_ast
                                            ],
                                        ),
                                    ],
                                ),
                            ]
                        ),
                    ],
                );
            }
        }
        return __PACKAGE__->new(
            %$self,
            ( $self->{block}
            ? ( block => Perlito5::AST::Block->new(
                        %{$self->{block}},
                        stmts => [ @stmts ],
                     ) )
            : () ),
        );
    }
}

package Perlito5::AST::Use;
{
    sub emit_compile_time {
        my $self = shift;
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
