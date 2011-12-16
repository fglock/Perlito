
class Perlito5::Precedence {

    has $.get_token;
    has $.reduce;
    has $.end_token;

    my $Operator = {};
    my $Precedence = {};    # integer 0..100
    my $Assoc = {};         # right, left, list
    my $Allow_space_before = {};

    sub is_assoc_type ($assoc_type, $op_name) {
        return $Assoc->{$assoc_type}{$op_name}
    }

    sub is_fixity_type ($fixity_type, $op_name) {
        return $Operator->{$fixity_type}{$op_name}
    }

    sub is_term ($token) {
        ($token->[0] eq 'term') || ($token->[0] eq 'postfix_or_term')
    }

    sub is_ident_middle ($c) {
           (($c ge 'a') && ($c le 'z'))
        || (($c ge '0') && ($c le '9'))
        ||  ($c eq '_')
    }

    my @Op;
    my $End_token;
    my @Op_chars = (3,2,1);
    method op_parse ($str, $pos) {
        my $from = $pos;
        for @($End_token) -> $tok {
            my $l = $tok.chars;
            my $s = substr($str, $pos, $l);
            if $s eq $tok {
                my $c1 = substr($str, $pos+$l-1, 1);
                my $c2 = substr($str, $pos+$l, 1);
                if is_ident_middle($c1) && ( is_ident_middle($c2) || $c2 eq '(' ) {
                }
                else {
                    return Perlito5::Match.new( 'str' => $str, 'from' => $from, 'to' => $pos+2, 'bool' => 1,
                        capture => ['end', $s] );
                }
            }
        }
        my $c01 = substr($str, $pos, 1);
        my $c02 = substr($str, $pos, 2);
        my $hyper_left = 0;
        my $hyper_right = 0;
        if ($c01 eq '«') || ($c01 eq '»') {
            $hyper_left = $c01;
            $pos = $pos + 1;
            $c02 = substr($str, $pos, 2);
        }
        elsif ($c02 eq '<<') || ($c02 eq '>>') {
            $hyper_left = $c02;
            $pos = $pos + 2;
            $c02 = substr($str, $pos, 2);
        }

        for @Op_chars -> $len {
            my $op = substr($str, $pos, $len);
            if exists(@Op[$len]{$op}) {
                my $c1 = substr($str, $pos+$len-1, 1);
                my $c2 = substr($str, $pos+$len, 1);
                if is_ident_middle($c1) && ( is_ident_middle($c2) || $c2 eq '(' ) {
                }
                else {
                    $pos = $pos + $len;
                    my $c01 = substr($str, $pos, 1);
                    my $c02 = substr($str, $pos, 2);
                    if ($c01 eq '«') || ($c01 eq '»') {
                        $hyper_right = $c01;
                        $pos = $pos + 1;
                    }
                    elsif ($c02 eq '<<') || ($c02 eq '>>') {
                        $hyper_right = $c02;
                        $pos = $pos + 2;
                    }
                    return Perlito5::Match.new( 'str' => $str, 'from' => $from, 'to' => $pos, 'bool' => 1,
                        capture => [ 'op', $op, { 'hyper_left' => $hyper_left, 'hyper_right' => $hyper_right } ] );
                }
            }
        }

        return Perlito5::Match.new( bool => 0 );
    }

    sub add_op ( $fixity, $name, $precedence, $param ) {
        if !(defined($param)) {
            $param = {}
        }
        my $assoc = $param->{'assoc'} || 'left';
        $Operator->{$fixity}{$name} = 1;
        $Precedence->{$name}        = $precedence;
        $Assoc->{$assoc}{$name}     = 1;
        $Allow_space_before->{$fixity}{$name} = $param->{'no_space_before'} ? False : True;
        @Op[ $name.chars ]{$name} = 1;
    }


    # s02:
    # - no space allowed before postfix ops
    # - if there is both an infix and a postfix with the same name, then the infix requires space before
    # - 'reduce' vs. 'array' [...]
    # - @a[] inside string interpolation
    # - longest token
    # - parentheses vs. Parcel (x) (x,)
    # - pair vs. block, hash vs. closure
    # - adverbs  1 == 100 :fuzz(3)
    # - function call without parentheses
    # - '|' in prefix position
    # - statement-ending blocks (S04)

    my $prec = 100;
    add_op( 'postfix', '.( )',               $prec, { no_space_before => True } );
    add_op( 'postfix', '.[ ]',               $prec, { no_space_before => True } );
    add_op( 'postfix', '.{ }',               $prec, { no_space_before => True } );
    add_op( 'postfix', '( )',                $prec, { no_space_before => True } );
    add_op( 'postfix', '[ ]',                $prec, { no_space_before => True } );
    add_op( 'postfix', 'funcall',            $prec, { no_space_before => True } );
    add_op( 'postfix', 'funcall_no_params',  $prec, { no_space_before => True } );
    add_op( 'postfix', 'methcall',           $prec, { no_space_before => True } );
    add_op( 'postfix', 'methcall_no_params', $prec, { no_space_before => True } );
    add_op( 'postfix', 'block',              $prec, { no_space_before => True } );
    add_op( 'postfix', 'hash',               $prec, { no_space_before => True } );
    $prec = $prec - 1;
    add_op( 'prefix',   '++',  $prec );
    add_op( 'prefix',   '--',  $prec );
    add_op( 'postfix',  '++',  $prec, { no_space_before => True } );
    add_op( 'postfix',  '--',  $prec, { no_space_before => True } );
    $prec = $prec - 1;
    add_op( 'infix',    '**',  $prec, { assoc => 'right' } );
    $prec = $prec - 1;
    add_op( 'prefix',   '+',   $prec );
    add_op( 'prefix',   '-',   $prec );
    add_op( 'prefix',   '$',   $prec );
    add_op( 'prefix',   '@',   $prec );
    add_op( 'prefix',   '%',   $prec );
    add_op( 'prefix',   '!',   $prec );
    add_op( 'prefix',   '?',   $prec );
    $prec = $prec - 1;
    add_op( 'infix',    '*',   $prec );
    add_op( 'infix',    '/',   $prec );
    $prec = $prec - 1;
    add_op( 'infix',    '+',   $prec );
    add_op( 'infix',    '-',   $prec );
    $prec = $prec - 1;
    add_op( 'infix',    'x',   $prec );
    add_op( 'infix',    'xx',  $prec );
    $prec = $prec - 1;
    add_op( 'infix',    '~',   $prec, { assoc => 'list' } );
    add_op( 'prefix',   '~',   $prec );
    $prec = $prec - 1;
    add_op( 'infix',    '&',   $prec, { assoc => 'list' } );
    add_op( 'prefix',   '&',   $prec );
    $prec = $prec - 1;
    add_op( 'infix',    '|',   $prec, { assoc => 'list' } );
    add_op( 'prefix',   '|',   $prec );
    $prec = $prec - 1;
    add_op( 'infix',    '<=>',  $prec );
    add_op( 'infix',    'leg',  $prec );
    add_op( 'infix',    'cmp',  $prec );
    add_op( 'infix',    'does', $prec );
    add_op( 'infix',    'but',  $prec );
    add_op( 'infix',    '..',   $prec );
    add_op( 'infix',    '^..',  $prec );
    add_op( 'infix',    '..^',  $prec );
    add_op( 'infix',    '^..^', $prec );

    $prec = $prec - 1;
    add_op( 'infix',    '===', $prec, { assoc => 'chain' } );
    add_op( 'infix',    'ne',  $prec, { assoc => 'chain' } );
    add_op( 'infix',    'eq',  $prec, { assoc => 'chain' } );
    add_op( 'infix',    'lt',  $prec, { assoc => 'chain' } );
    add_op( 'infix',    'le',  $prec, { assoc => 'chain' } );
    add_op( 'infix',    'gt',  $prec, { assoc => 'chain' } );
    add_op( 'infix',    'ge',  $prec, { assoc => 'chain' } );
    add_op( 'infix',    '<=',  $prec, { assoc => 'chain' } );
    add_op( 'infix',    '>=',  $prec, { assoc => 'chain' } );
    add_op( 'infix',    '==',  $prec, { assoc => 'chain' } );
    add_op( 'infix',    '!=',  $prec, { assoc => 'chain' } );
    add_op( 'infix',    '<',   $prec, { assoc => 'chain' } );
    add_op( 'infix',    '>',   $prec, { assoc => 'chain' } );
    add_op( 'infix',    '~~',  $prec, { assoc => 'chain' } );
    $prec = $prec - 1;
    add_op( 'infix',    '&&',  $prec );
    $prec = $prec - 1;
    add_op( 'infix',    '||',  $prec );
    add_op( 'infix',    '//',  $prec );
    $prec = $prec - 1;
    add_op( 'ternary',  '?? !!',  $prec );
    add_op( 'ternary',  '? :',  $prec );
    $prec = $prec - 1;
    add_op( 'infix',    '=',   $prec, { assoc => 'right' } );
    add_op( 'infix',    ':=',  $prec, { assoc => 'right' } );

    add_op( 'infix',    '||=',   $prec, { assoc => 'right' } );
    add_op( 'infix',    '&&=',   $prec, { assoc => 'right' } );
    add_op( 'infix',    '|=',   $prec, { assoc => 'right' } );
    add_op( 'infix',    '&=',   $prec, { assoc => 'right' } );
    add_op( 'infix',    '//=',   $prec, { assoc => 'right' } );
    add_op( 'infix',    '+=',   $prec, { assoc => 'right' } );
    add_op( 'infix',    '-=',   $prec, { assoc => 'right' } );
    add_op( 'infix',    '*=',   $prec, { assoc => 'right' } );
    add_op( 'infix',    '/=',   $prec, { assoc => 'right' } );
    add_op( 'infix',    '~=',   $prec, { assoc => 'right' } );

    $prec = $prec - 1;
    add_op( 'prefix',   'not', $prec );
    $prec = $prec - 1;
    add_op( 'infix',    '=>',  $prec );
    $prec = $prec - 1;
    add_op( 'list',     ',',   $prec, { assoc => 'list' } );
    $prec = $prec - 1;
    # TODO - semicolon
    # add_op( 'list',     ';',   $prec, { assoc => 'list' } );
    # $prec = $prec - 1;
    add_op( 'infix',    'and', $prec );
    $prec = $prec - 1;
    add_op( 'infix',    'or',  $prec );
    $prec = $prec - 1;
    add_op( 'infix',    '*start*', $prec );

    method precedence_parse {
        my $get_token = self.get_token;
        my $reduce    = self.reduce;
        my $last_end_token = $End_token;
        $End_token    = self.end_token;
        my $op_stack  = [];   # [category, name]
        my $num_stack = [];
        my $last      = ['op', '*start*'];
        my $last_has_space = False;
        my $token     = $get_token.();
        # say "# precedence get_token: (0) ", $token.perl;
        if ($token->[0]) eq 'space' {
            $token = $get_token.()
        }
        while (defined($token)) && ($token->[0] ne 'end') {
            # say "# precedence      last: (1) ", $last.perl;
            # say "# precedence get_token: (1) ", $token.perl;
            if ($token->[1] eq ',') && ( ($last->[1] eq '*start*') || ($last->[1] eq ',') ) {
                # allow (,,,)
                $num_stack.push( ['term', Mu] );
            }
            if $Operator->{'prefix'}{$token->[1]} && ( ($last->[1] eq '*start*') || !(is_term($last)) ) {
                $token->[0] = 'prefix';
                $op_stack.unshift($token);
            }
            elsif $Operator->{'postfix'}{$token->[1]} && is_term($last)
                && (  $Allow_space_before->{'postfix'}{$token->[1]}
                   || !($last_has_space)
                   )
            {
                my $pr = $Precedence->{$token->[1]};
                while $op_stack.elems && ($pr <= $Precedence->{ ($op_stack->[0])[1] }) {
                    $reduce.($op_stack, $num_stack);
                }
                if ($token->[0]) ne 'postfix_or_term' {
                    $token->[0] = 'postfix';
                }
                $op_stack.unshift($token);
            }
            elsif ($token->[1] eq 'block') && is_term($last) && $last_has_space {
                # a block in this position terminates the current expression
                # say "# there is a block after the expression: ", $token.perl;
                while $op_stack.elems() {
                    $reduce.($op_stack, $num_stack);
                }
                $num_stack.push($token);  # save the block
                $End_token = $last_end_token;  # restore previous 'end token' context
                return $num_stack;
            }
            elsif is_term($token) {
                # say "# ** two terms in a row ";
                # say "#      last:  ", $last.perl;
                # say "#      token: ", $token.perl;
                # say "#      space: ", $last_has_space;
                if is_term($last) {
                    say "#      last:  ", $last.perl;
                    say "#      token: ", $token.perl;
                    say "#      space: ", $last_has_space;
                    die "Value tokens must be separated by an operator";
                }
                $token->[0] = 'term';
                $num_stack.push($token);
            }
            elsif $Precedence->{$token->[1]} {
                my $pr = $Precedence->{$token->[1]};
                if $Assoc->{'right'}{$token->[1]} {
                    while $op_stack.elems && ( $pr < $Precedence->{ ($op_stack->[0])[1] } ) {
                        $reduce.($op_stack, $num_stack);
                    }
                }
                else {
                    while $op_stack.elems && ( $pr <= $Precedence->{ ($op_stack->[0])[1] } ) {
                        $reduce.($op_stack, $num_stack);
                    }
                }
                if $Operator->{'ternary'}{$token->[1]} {
                    $token->[0] = 'ternary';
                }
                else {
                    $token->[0] = 'infix';
                }
                $op_stack.unshift($token);
            }
            else {
                die "Unknown token: '", $token->[1], "'";
            }
            $last = $token;
            $token = $get_token.();
            # say "# precedence get_token: (2) ", $token.perl;
            if $token->[0] eq 'space' {
                $token = $get_token.();
                $last_has_space = True;
            }
            else {
                $last_has_space = False;
            }
        }
        if defined($token) && ($token->[0] ne 'end') {
            die "Unexpected end token: ",$token.perl;
        }
        while $op_stack.elems() {
            $reduce.($op_stack, $num_stack);
        }
        # say "# precedence return";
        $End_token = $last_end_token;  # restore previous 'end token' context
        return $num_stack;
    }
}

=begin

=head1 NAME

Perlito5::Precedence - precedence parser for Perlito

=head1 SYNOPSIS

    my $prec = Perlito5::Precedence.new(
        get_token => $get_token,
        reduce    => $reduce_to_ast,
        end_token => [ ']', ')', '}', ';' ] );
    my $res = $prec.precedence_parse;

=head1 DESCRIPTION

This module resolves the operator precedence in Perl 6 expressions.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2010, 2011 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

