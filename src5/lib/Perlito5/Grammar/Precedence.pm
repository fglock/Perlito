
package Perlito5::Grammar::Precedence;

use feature 'say';
use strict;

# Perlito5::Grammar::Precedence attributes:
#   get_token - code ref
#   reduce    - code ref
#   end_token - array ref
#   end_token_chars - array ref (index to end_token entries)

sub new { 
    my $class = shift;
    bless {@_}, $class
}

my $Operator         = {};
my $Precedence       = {};  # integer 0..100
my $PrefixPrecedence = {};  # integer 0..100; 'prefix' operations
my $Assoc            = {};  # right, left, list

sub is_assoc_type {
    my $assoc_type = shift;
    my $op_name = shift;
    return $Assoc->{$assoc_type}{$op_name}
}

sub is_fixity_type {
    my $fixity_type = shift;
    my $op_name = shift;
    return $Operator->{$fixity_type}{$op_name}
}

sub is_term {
    my $token = shift;
    ($token->[0] eq 'term') || ($token->[0] eq 'postfix_or_term') || ($token->[0] eq 'postfix')
}

sub is_num {
    $_[0] ge '0' && $_[0] le '9'
}

sub is_ident_middle {
    my $c = shift;
       ($c ge 'a' && $c le 'z')
    || ($c ge 'A' && $c le 'Z')
    || ($c ge '0' && $c le '9')
    || ($c eq '_')
}

my @Parsed_op_chars = (2, 1);
my %Parsed_op = (
      # 1 char
        '?'  => sub { Perlito5::Grammar::Expression::term_ternary($_[0], $_[1]) },
        '('  => sub { Perlito5::Grammar::Expression::term_paren($_[0], $_[1]) },
        '['  => sub { Perlito5::Grammar::Expression::term_square($_[0], $_[1]) },
        '{'  => sub { Perlito5::Grammar::Expression::term_curly($_[0], $_[1]) },
      # 2 chars
        '->' => sub { Perlito5::Grammar::Expression::term_arrow($_[0], $_[1]) },
);

my @Term_chars;
my %Term;
sub add_term {
    my $name = shift;
    my $param = shift;

    $Term{$name} = $param;
    unshift @Term_chars, scalar(@Term_chars) + 1
        while @Term_chars < length($name);
}

my $End_token;
my $End_token_chars;
my %Op;
my @Op_chars = (3,2,1);
sub op_parse {
    my $str  = shift;
    my $pos  = shift;
    my $last_is_term = shift;

    my $tok = join( "", @{$str}[ $pos .. $pos + 10 ] );

    for my $len ( @$End_token_chars ) {
        my $term = substr($tok, 0, $len);
        if (exists($End_token->{$term})) {
            my $c1 = $str->[$pos + $len - 1];
            my $c2 = $str->[$pos + $len];
            if (  !(is_ident_middle($c1) && is_ident_middle($c2) )
               && !($c1 eq '<' && $c2 eq '<')
               )
            {
                # it looks like an end token, and it is not one of these cases:
                #   if_more
                return {
                    str     => $str,
                    from    => $pos,
                    to      => $pos,
                    capture => [ 'end', $term ]
                };
            }
        }
    }

    if ( !$last_is_term ) {
        for my $len ( @Term_chars ) {
            my $term = substr($tok, 0, $len);
            if (exists($Term{$term})) {
                my $c1 = $str->[$pos + $len - 1];
                my $c2 = $str->[$pos + $len];
                if ( is_num($c1) || !is_ident_middle($c1) || !is_ident_middle($c2) ) {
                    my $m = $Term{$term}->($str, $pos);
                    return $m if $m;
                }
            }
        }
    }

    # check for operators that need special parsing
    for my $len ( @Parsed_op_chars ) {
        my $op = substr($tok, 0, $len);
        if (exists($Parsed_op{$op})) {
            my $m = $Parsed_op{$op}->($str, $pos);
            return $m if $m;
        }
    }

    for my $len ( @Op_chars ) {
        my $op = substr($tok, 0, $len);
        if (exists($Op{$op})) {
            my $c1 = $str->[$pos + $len - 1];
            my $c2 = $str->[$pos + $len];
            if (   (  !(is_ident_middle($c1) && is_ident_middle($c2))   # "and" can't be followed by "_"
                   && !($c1 eq '&' && $c2 eq '&')                       # "&" can't be followed by "&"
                   ) 
                || (  $c1 eq 'x' && $c2 ge '0' && $c2 le '9'            # "x3" is ok, parses as "x 3"
                   )
               )
            {
                # it looks like an operator, and it is not one of these cases:
                #   and -> and_more
                #   &   -> &&
                #   x   -> x3  ok!

                if (  exists($Operator->{infix}{$op}) 
                   && !exists($Operator->{prefix}{$op})
                   && !$last_is_term
                   )
                {
                    # only allows an infix after last_is_term
                }
                else {
                    return {
                        str     => $str,
                        from    => $pos,
                        to      => $pos + $len,
                        capture => [ 'op', $op ]
                    };
                }
            }
        }
    }

    return Perlito5::Grammar::Bareword::term_bareword( $str, $pos );
}

sub add_op {
    my ($fixity, $names, $precedence, $param) = @_;
    $param //= {};
    my $assoc = $param->{assoc} || 'left';
    for my $name ( @$names ) {
        $Operator->{$fixity}{$name} = 1;
        $Precedence->{$name}        = $precedence;
        $PrefixPrecedence->{$name}  = $precedence if $fixity eq 'prefix';
        $Assoc->{$assoc}{$name}     = 1;
        $Op{$name}                  = 1;
    }
}

# left        terms and list operators (leftward)
# left        ->
# nonassoc    ++ --
# right       **
# right       ! ~ \ and unary + and -
# left        =~ !~
# left        * / % x
# left        + - .
# left        << >>
# nonassoc    named unary operators
# nonassoc    < > <= >= lt gt le ge
# nonassoc    == != <=> eq ne cmp ~~
# left        &
# left        | ^
# left        &&
# left        || //
# nonassoc    ..  ...
# right       ?:
# right       = += -= *= etc.
# left        , =>
# nonassoc    list operators (rightward)
# right       not
# left        and
# left        or xor

my $prec = 100;
add_op( 'postfix',
    [ '.( )', '.[ ]', '.{ }', '( )', '[ ]',
      'funcall', 'funcall_no_params', 'methcall', 'methcall_no_params', 'block', 'hash',
    ],
    $prec, );
$prec = $prec - 1;
add_op( 'prefix',  [ '++',  '--' ], $prec );
add_op( 'postfix', [ '++',, '--' ], $prec, );
$prec = $prec - 1;
add_op( 'infix', [ '**' ],  $prec, { assoc => 'right' } );
$prec = $prec - 1;
add_op( 'prefix', [ '\\', '+', '-', '~', '!' ], $prec );
$prec = $prec - 1;
add_op( 'infix', [ '=~', '!~' ], $prec );
$prec = $prec - 1;
add_op( 'infix', [ '*', '/', '%', 'x' ], $prec );
$prec = $prec - 1;
add_op( 'infix', [ '+', '-' ], $prec );
add_op( 'infix', [ '.' ],   $prec, { assoc => 'list' } );
$prec = $prec - 1;
add_op( 'infix', [ '<<', '>>' ], $prec );
$prec = $prec - 1;
# named unary operators - these are parsed by the "Grammar::Bareword" module
# NOTE -  -f($file).".bak" is equivalent to -f "$file.bak" 
add_op( 'prefix', [qw( -r -w -x -o -R -W -X -O -e -z -s -f -d -l -p -S -b -c -t -u -g -k -T -B -M -A -C )], $prec );
$prec = $prec - 1;
add_op( 'infix', [ 'lt', 'le', 'gt', 'ge', '<=', '>=', '<', '>' ], $prec, { assoc => 'chain' } );
$prec = $prec - 1;
add_op( 'infix', [ '<=>', 'cmp', '==', '!=', 'ne', 'eq' ], $prec, { assoc => 'chain' } );
$prec = $prec - 1;
add_op( 'infix', [ '&' ],   $prec );
$prec = $prec - 1;
add_op( 'infix', [ '|', '^' ], $prec );
$prec = $prec - 1;
add_op( 'infix', [ '..', '...' ], $prec );
add_op( 'infix', [ '~~' ],  $prec, { assoc => 'chain' } );
$prec = $prec - 1;
add_op( 'infix', [ '&&' ],  $prec, { assoc => 'right' } );
$prec = $prec - 1;
add_op( 'infix', [ '||' ],  $prec, { assoc => 'right' } );
add_op( 'infix', [ '//' ],  $prec );
$prec = $prec - 1;
add_op( 'ternary', [ '? :' ],  $prec, { assoc => 'right' } );
$prec = $prec - 1;
add_op(
    'infix',
    [ '=', '**=', '+=', '-=', '*=', '/=', 'x=', '|=', '&=', '.=', '<<=', '>>=', '%=', '||=', '&&=', '^=', '//=' ],
    $prec,
    { assoc => 'right' } );
$prec = $prec - 1;
add_op( 'infix', [ '=>' ],  $prec );
$prec = $prec - 1;
add_op( 'list', [ ',' ],   $prec, { assoc => 'list' } );
$prec = $prec - 1;
add_op( 'prefix', [ 'not' ], $prec );
$prec = $prec - 1;
add_op( 'infix', [ 'and' ], $prec );
$prec = $prec - 1;
add_op( 'infix', [ 'or', 'xor' ], $prec );
$prec = $prec - 1;
add_op( 'infix', [ '*start*' ], $prec );


sub get_token_precedence {
    my $token = $_[0];
    if ( $token->[0] eq 'prefix' ) {
        return $PrefixPrecedence->{ $token->[1] }
    }
    return $Precedence->{ $token->[1] }
}

sub precedence_parse {

    # this routine implements operator precedence
    # using (more or less) the "shunting yard" algorithm
    #
    # the parsing process is based on Perl 6:
    # "Perl 6 "sandwiches" an operator-precedence parser in between two Recursive descent parsers"
    #
    # see the "token" implementation for the recursive descent parser (Perlito5::Grammar::Regex6)
    #
    # http://en.wikipedia.org/wiki/Operator-precedence_parser
    # http://en.wikipedia.org/wiki/Shunting-yard_algorithm
    #

    my $self = shift;

    my $get_token       = $self->{get_token};
    my $reduce          = $self->{reduce};
    my $last_end_token  = $End_token;
    my $last_end_token_chars = $End_token_chars;
    $End_token          = $self->{end_token};
    $End_token_chars    = $self->{end_token_chars};
    my $op_stack  = [];   # [category, name]
    my $num_stack = [];
    my $last      = ['op', '*start*'];
    my $last_is_term = 0;
    my $token     = $get_token->($last_is_term);
    if ($token->[0] eq 'space') {
        $token = $get_token->($last_is_term)
    }
    while ((defined($token)) && ($token->[0] ne 'end')) {
        my $token_is_term = is_term($token);
        if (($token->[1] eq ',') && ( ($last->[1] eq '*start*') || ($last->[1] eq ',') )) {
            # allow (,,,)
            push( @$num_stack, ['term', undef] );
        }
        if ($Operator->{prefix}{$token->[1]} && ( ($last->[1] eq '*start*') || !$last_is_term )) {
            $token->[0] = 'prefix';
            unshift( @$op_stack, $token);
        }
        elsif ( $Operator->{postfix}{$token->[1]} && $last_is_term )
        {
            my $pr = $Precedence->{$token->[1]};
            while (scalar(@$op_stack) && ($pr <= get_token_precedence($op_stack->[0]))) {
                $reduce->($op_stack, $num_stack);
            }
            if ($token->[0] ne 'postfix_or_term') {
                $token->[0] = 'postfix';
            }
            unshift( @$op_stack, $token);
            # note: from the point of view os the tokenizer, a postfix is a term:
            #       '$step++ < $steps', there is a term before '<'
            $token_is_term = 1;
        }
        elsif ($token_is_term) {
            if ($last_is_term) {
                # print "#      last:  ", Data::Dumper::Dumper($last);
                # print "#      token: ", Data::Dumper::Dumper($token);
                Perlito5::Compiler::error( "Value tokens must be separated by an operator (did you forget a comma?)" );
            }
            $token->[0] = 'term';
            push( @$num_stack, $token);
        }
        elsif ($Precedence->{$token->[1]}) {
            my $pr = $Precedence->{$token->[1]};
            if ($Assoc->{right}{$token->[1]}) {
                while (scalar(@$op_stack) && ( $pr < get_token_precedence($op_stack->[0]))) {
                    $reduce->($op_stack, $num_stack);
                }
            }
            else {
                while (scalar(@$op_stack) && ( $pr <= get_token_precedence($op_stack->[0]))) {
                    $reduce->($op_stack, $num_stack);
                }
            }
            if ($Operator->{ternary}{$token->[1]}) {
                $token->[0] = 'ternary';
            }
            else {
                $token->[0] = 'infix';
            }
            unshift( @$op_stack, $token);
        }
        else {
            Perlito5::Compiler::error( "Unknown token: '", $token->[1], "'" );
        }
        $last = $token;
        $last_is_term = $token_is_term;
        $token = $get_token->($last_is_term);
        if ($token->[0] eq 'space') {
            $token = $get_token->($last_is_term);
        }
    }
    if (defined($token) && ($token->[0] ne 'end')) {
        Perlito5::Compiler::error( "Unexpected end token: ", $token );
    }
    while (scalar(@$op_stack)) {
        $reduce->($op_stack, $num_stack);
    }
    # say "# precedence return";
    $End_token = $last_end_token;  # restore previous 'end token' context
    $End_token_chars = $last_end_token_chars;  # restore previous 'end token' context
    return $num_stack;
}

1;

=begin

=head1 NAME

Perlito5::Grammar::Precedence - precedence parser for Perlito

=head1 SYNOPSIS

    my $prec = Perlito5::Grammar::Precedence->new(
        get_token => $get_token,
        reduce    => $reduce_to_ast,
        end_token => [ 
                {}, 
                { ']' => 1, ')' => 1, '}' => 1, ';' => 1 } 
            ],
        end_token_chars => [ 1 ],
    );
    my $res = $prec->precedence_parse;

=head1 DESCRIPTION

This module resolves the operator precedence in Perl 5 expressions.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2010, 2011, 2012 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

