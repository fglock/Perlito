use v5;
package Perlito5::Macro;
use strict;


sub empty_while_filehandle {
    my $self = $_[0];

    # The loop
    #
    #     while (<>) {
    #         ...                     # code for each line
    #     }
    #
    # is equivalent to the following Perl-like pseudo code:
    #
    #     unshift(@ARGV, '-') unless @ARGV;
    #     while ($ARGV = shift) {
    #         open(ARGV, $ARGV) or die $!;
    #         while (<ARGV>) {
    #             ...         # code for each line
    #         }
    #     }
    #

    # Process the input:
    #   bless({
    #       'cond' => bless({
    #           'arguments' => [],
    #           'code' => '<glob>',
    #           'namespace' => '',
    #       }, 'Perlito5::AST::Apply'),
    #       'body' => ...
    #       'continue' => ...
    #   }, 'Perlito5::AST::While');
    #
    return if !($self->isa('Perlito5::AST::While'));
    return if !($self->{cond}->isa('Perlito5::AST::Apply'));
    return if !($self->{cond}->{code} eq '<glob>');
    return if !(@{ $self->{cond}->{arguments} } == 0);
    my $body = $self->{body};
    my $continue = $self->{continue};

    return 
            bless({
                'cond' => bless({
                    'name' => 'ARGV',
                    'namespace' => '',
                    'sigil' => '@',
                }, 'Perlito5::AST::Var'),
                'otherwise' => bless({
                    'arguments' => [
                        bless({
                            'name' => 'ARGV',
                            'namespace' => '',
                            'sigil' => '@',
                        }, 'Perlito5::AST::Var'),
                        bless({
                            'buf' => '-',
                        }, 'Perlito5::AST::Val::Buf'),
                    ],
                    'code' => 'unshift',
                    'namespace' => '',
                }, 'Perlito5::AST::Apply'),
            }, 'Perlito5::AST::If'),

            bless({
                'body' => bless({
                    'sig' => undef,
                    'stmts' => [
                        bless({
                            'arguments' => [
                                bless({
                                    'arguments' => [
                                        bless({
                                            'arguments' => [],
                                            'bareword' => 1,
                                            'code' => 'ARGV',
                                            'namespace' => '',
                                        }, 'Perlito5::AST::Apply'),
                                        bless({
                                            'name' => 'ARGV',
                                            'namespace' => '',
                                            'sigil' => '$',
                                        }, 'Perlito5::AST::Var'),
                                    ],
                                    'code' => 'open',
                                    'namespace' => '',
                                }, 'Perlito5::AST::Apply'),
                                bless({
                                    'arguments' => [
                                        bless({
                                            'name' => '!',
                                            'namespace' => '',
                                            'sigil' => '$',
                                        }, 'Perlito5::AST::Var'),
                                    ],
                                    'code' => 'die',
                                    'namespace' => '',
                                }, 'Perlito5::AST::Apply'),
                            ],
                            'code' => 'infix:<or>',
                            'namespace' => '',
                        }, 'Perlito5::AST::Apply'),
                        bless({
                            'body' => $body,
                            'cond' => bless({
                                'arguments' => [
                                    bless({
                                        'arguments' => [],
                                        'bareword' => 1,
                                        'code' => 'ARGV',
                                        'namespace' => '',
                                    }, 'Perlito5::AST::Apply'),
                                ],
                                'code' => '<glob>',
                                'namespace' => '',
                            }, 'Perlito5::AST::Apply'),
                            'continue' => $continue,
                        }, 'Perlito5::AST::While'),
                    ],
                }, 'Perlito5::AST::Lit::Block'),
                'cond' => bless({
                    'arguments' => [
                        bless({
                            'name' => 'ARGV',
                            'namespace' => '',
                            'sigil' => '$',
                        }, 'Perlito5::AST::Var'),
                        bless({
                            'arguments' => [],
                            'bareword' => 1,
                            'code' => 'shift',
                            'namespace' => '',
                        }, 'Perlito5::AST::Apply'),
                    ],
                    'code' => 'infix:<=>',
                    'namespace' => '',
                }, 'Perlito5::AST::Apply'),
                'continue' => bless({
                    'sig' => undef,
                    'stmts' => [],
                }, 'Perlito5::AST::Lit::Block'),
            }, 'Perlito5::AST::While');

}


package Perlito5::AST::Apply;

my %op = (
    'infix:<+=>'  => 'infix:<+>',
    'infix:<-=>'  => 'infix:<->',
    'infix:<*=>'  => 'infix:<*>',
    'infix:</=>'  => 'infix:</>',
    'infix:<||=>' => 'infix:<||>',
    'infix:<&&=>' => 'infix:<&&>',
    'infix:<|=>'  => 'infix:<|>',
    'infix:<&=>'  => 'infix:<&>',
    'infix:<//=>' => 'infix:<//>',
    'infix:<.=>'  => 'list:<.>',
);

sub op_assign {
    my $self = $_[0];

    my $code = $self->{code};
    return 0 if ref($code);

    if (exists( $op{$code} )) {
        return Perlito5::AST::Apply->new(
            code      => 'infix:<=>',
            arguments => [
                $self->{arguments}->[0],
                Perlito5::AST::Apply->new(
                    code      => $op{$code},
                    arguments => $self->{arguments},
                ),
            ]
        );
    }

    return 0;
}


package Perlito5::AST::Do;
sub simplify {
    my $self = $_[0];

    my $block;
    if ($self->{block}->isa('Perlito5::AST::Lit::Block')) {
        $block = $self->{block}->stmts;
    }
    else {
        $block = [ $self->{block} ]
    }
    if (scalar(@$block) == 1) {
        # optimize some code generated by the regex compiler
        my $stmt = $block->[0];
        if ($stmt->isa('Perlito5::AST::Apply') && $stmt->code() eq 'circumfix:<( )>') {
            my $args = $stmt->arguments;
            return Perlito5::AST::Do->new( block => $args->[0] )->simplify
                if @$args == 1;
            # do {()}
            return Perlito5::AST::Do->new( block => $block );
        }
        if ($stmt->isa('Perlito5::AST::Do')) {
            return $stmt->simplify;
        }
    }
    return Perlito5::AST::Do->new( block => $block );
}

=begin

=head1 NAME

Perlito5::Macro - Ast macros for Perlito

=head1 SYNOPSIS

    $ast = $ast.op_assign()

=head1 DESCRIPTION

This module implements some Ast transformations for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2011, 2012 by Flavio Soibelmann Glock.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

