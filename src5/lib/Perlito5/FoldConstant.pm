
package Perlito5::FoldConstant;

use v5;
use Perlito5::AST;
use Perlito5::Dumper;
use strict;

sub fold_constant {
    my $self = shift;
    my $ref = ref($self);
    return $self
        if     $ref eq 'Perlito5::AST::Int'
            || $ref eq 'Perlito5::AST::Num'
            || $ref eq 'Perlito5::AST::Buf';
 
    if ($ref eq 'Perlito5::AST::Apply' ) {

        for my $pos ( 0 .. $#{ $self->{arguments} } ) {
            $self->{arguments}[$pos] = fold_constant($self->{arguments}[$pos]);
        }

        my $code = $self->{code};
        my ($arg0, $arg1) = @{ $self->{arguments} };

        if ($code eq 'infix:<+>') {
            if (is_constant($arg0) && is_constant($arg1)) {
                my $v = $arg0->value + $arg1->value;
                if ($v == int($v)) {
                    return Perlito5::AST::Int->new(int => $v);
                }
                return Perlito5::AST::Num->new(num => $v);
            }
        }
        if ($code eq 'prefix:<->') {
            if (is_constant($arg0)) {
                my $v = -$arg0->value;
                if ( ref($arg0) eq 'Perlito5::AST::Buf' ) {
                    # negate string
                    return Perlito5::AST::Buf->new(buf => $v);
                }
                if ( ref($arg0) eq 'Perlito5::AST::Int' ) {
                    return Perlito5::AST::Int->new(int => $v);
                }
                return Perlito5::AST::Num->new(num => $v);
            }
        }
        if ($code eq 'infix:<*>') {
            if (is_constant($arg0) && is_constant($arg1)) {
                my $v = $arg0->value * $arg1->value;
                if ($v == int($v)) {
                    return Perlito5::AST::Int->new(int => $v);
                }
                return Perlito5::AST::Num->new(num => $v);
            }
        }
        if ($code eq 'infix:</>') {
            if (is_constant($arg0) && is_constant($arg1)) {
                my $v = $arg0->value / $arg1->value;
                if ($v == int($v)) {
                    return Perlito5::AST::Int->new(int => $v);
                }
                return Perlito5::AST::Num->new(num => $v);
            }
        }
        if ($code eq 'infix:<**>') {
            if (is_constant($arg0) && is_constant($arg1)) {
                my $v = $arg0->value ** $arg1->value;
                # if ($v == int($v)) {
                #     return Perlito5::AST::Int->new(int => $v);
                # }
                return Perlito5::AST::Num->new(num => $v);
            }
        }
        if ($code eq 'infix:<!=>') {
            if (is_constant($arg0) && is_constant($arg1)) {
                my $v = $arg0->value != $arg1->value;
                if ($v) {
                    return Perlito5::AST::Int->new(int => 1);
                }
                return Perlito5::AST::Apply->UNDEF();
            }
        }
        if ($code eq 'infix:<==>') {
            if (is_constant($arg0) && is_constant($arg1)) {
                my $v = $arg0->value == $arg1->value;
                if ($v) {
                    return Perlito5::AST::Int->new(int => 1);
                }
                return Perlito5::AST::Apply->UNDEF();
            }
        }
        if ($code eq 'prefix:<!>' || $code eq 'prefix:<not>' ) {
            if (is_constant($arg0)) {
                my $v = !$arg0->value;
                if ($v) {
                    return Perlito5::AST::Int->new(int => 1);
                }
                return Perlito5::AST::Apply->UNDEF();
            }
        }
        if ($code eq 'infix:<&&>' || $code eq 'infix:<and>' ) {
            if (is_constant($arg0)) {
                if ($arg0->value) {
                    return $arg1;
                }
                return $arg0;
            }
        }
        if ($code eq 'infix:<||>' || $code eq 'infix:<or>' ) {
            if (is_constant($arg0)) {
                if ($arg0->value) {
                    return $arg0;
                }
                return $arg1;
            }
        }
        if ($code eq 'int') {
            if (is_constant($arg0)) {
                return Perlito5::AST::Int->new(int => int($arg0->value));
            }
        }
        if ($code eq 'ord') {
            if (is_constant($arg0)) {
                return Perlito5::AST::Int->new(int => ord($arg0->value));
            }
        }
        if ($code eq 'chr') {
            if (is_constant($arg0)) {
                return Perlito5::AST::Buf->new(buf => chr($arg0->value));
            }
        }

        if (my $const = $Perlito5::CONSTANT{ $self->{namespace} . '::' . $code }) {
            return $const;
        }
    }
    return $self;
}

sub is_constant {
    my $self = shift;
    my $ref = ref($self);
    return     $ref eq 'Perlito5::AST::Int'
            || $ref eq 'Perlito5::AST::Num'
            || $ref eq 'Perlito5::AST::Buf'
            || ( $ref eq 'Perlito5::AST::Apply' && $self->{code} eq 'undef' && !@{$self->{arguments}});
}
 
1;

