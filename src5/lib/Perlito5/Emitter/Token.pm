use v5;

# no <after .. > - so it doesn't need to match backwards
# no backtracking on quantifiers

class Rul {

    sub constant {
            my $str = shift;
            my $len = length $str;
            if ($str eq '\\') {
                $str = '\\\\';
            }
            if ($str eq '\'') {
                $str = '\\\'';
            }
            if ( $len ) {
                '( \'' . $str . '\' eq substr( $str, $MATCH->to, ' . $len . ') ' .
                '&& ( $MATCH->to = ' . $len . ' + $MATCH->to )' .
                ')';
            }
            else {
                return '1'
            }
    }
}

class Rul::Quantifier {
    has $.term;
    has $.quant;
    has $.greedy;
    has $.ws1;
    has $.ws2;
    has $.ws3;
    sub emit_perl5 {
        my $self = $_[0];

        if (($.quant eq '') && ($.greedy eq '')) {
            return $.term->emit_perl5;
        }
        if (($.quant eq '+') && ($.greedy eq '')) {
            $.term->set_captures_to_array;
            return
                '(do { '
                .   'my $last_match_null = 0; '
                .   'my $last_pos = $MATCH->to; '
                .   'my $count = 0; '
                .   'while (' . $.term->emit_perl5() . ' && ($last_match_null < 2)) '
                .   '{ '
                .       'if ($last_pos == $MATCH->to()) { '
                .           '$last_match_null = $last_match_null + 1; '
                .       '} '
                .       'else { '
                .           '$last_match_null = 0; '
                .       '}; '
                .       '$last_pos = $MATCH->to; '
                .       '$count = $count + 1; '
                .   '}; '
                .   '$MATCH->to = $last_pos; '
                .   '$count > 0; '
                . '})';
        }
        if (($.quant eq '*') && ($.greedy eq '')) {
            $.term->set_captures_to_array;
            return
                '(do { '
                .   'my $last_match_null = 0; '
                .   'my $last_pos = $MATCH->to; '
                .   'while (' . $.term->emit_perl5() . ' && ($last_match_null < 2)) '
                .   '{ '
                .       'if ($last_pos == $MATCH->to()) { '
                .           '$last_match_null = $last_match_null + 1; '
                .       '} '
                .       'else { '
                .           '$last_match_null = 0; '
                .       '}; '
                .       '$last_pos = $MATCH->to; '
                .   '}; '
                .   '$MATCH->to = $last_pos; '
                .   '1 '
                . '})';
        }
        if (($.quant eq '?') && ($.greedy eq '')) {
            $.term->set_captures_to_array;
            return
                '(do { '
                .   'my $last_pos = $MATCH->to; '
                .   'if (!(do {' . $.term->emit_perl5() . '})) '
                .   '{ '
                .       '$MATCH->to = $last_pos; '
                .   '}; '
                .   '1 '
                . '})';
        }

        # TODO
        warn "Rul::Quantifier:  not implemented";
        $.term->emit_perl5;
    }
    sub set_captures_to_array {
        my $self = $_[0];

        $.term->set_captures_to_array;
    }
}

class Rul::Or {
    has $.or_list;
    sub emit_perl5 {
        my $self = $_[0];

        '(do { '
            . 'my $pos1 = $MATCH->to; (do { '
            . join( '}) || (do { $MATCH->to = $pos1; ',
                  map( $_->emit_perl5, @{$.or_list} )
                )
        . '}) })';
    }
    sub set_captures_to_array {
        my $self = $_[0];

        map( $_->set_captures_to_array, @{$.or_list} );
    }
}

class Rul::Concat {
    has $.concat;
    sub emit_perl5 {
        my $self = $_[0];

        '('
            . join( ' && ',
                    map( $_->emit_perl5, @{$.concat} )
                  )
        . ')';
    }
    sub set_captures_to_array {
        my $self = $_[0];

        map( $_->set_captures_to_array, @{$.concat} );
    }
}

class Rul::Subrule {
    has $.metasyntax;
    has $.captures;
    sub emit_perl5 {
        my $self = $_[0];

        my $meth = ( 1 + index( $.metasyntax, '.' ) )
            ? Perlito5::Runtime::_replace( $.metasyntax, '.', '->' )
            : ( '$grammar->' . $.metasyntax );

        my $code;
        if ($.captures == 1) {
            $code = 'if ($m2->bool) { $MATCH->to = $m2->to; $MATCH->{\'' . $.metasyntax . '\'} = $m2; 1 } else { 0 }; '
        }
        elsif ($.captures > 1) {
            # TODO: capture level > 2
            $code = 'if ($m2->bool) { '
                    .   '$MATCH->to = $m2->to; '
                    .   'if (exists $MATCH->{\'' . $.metasyntax . '\'}) { '
                    .       'push @{ $MATCH->{\'' . $.metasyntax . '\'} }, $m2; '
                    .   '} '
                    .   'else { '
                    .       '$MATCH->{\'' . $.metasyntax . '\'} = [ $m2 ]; '
                    .   '}; '
                    .   '1 '
                    . '} else { 0 }; '
        }
        else {
            $code = 'if ($m2->bool) { $MATCH->to = $m2->to; 1 } else { 0 }; '
        }

        '(do { '
        .   'my $m2 = ' . $meth . '($str, $MATCH->to); '
        .   $code
        . '})'
    }
    sub set_captures_to_array {
        my $self = $_[0];

        if ($.captures > 0) {
            $.captures = $.captures + 1;
        }
    }
}

class Rul::Var {
    has $.sigil;
    has $.twigil;
    has $.name;
    sub emit_perl5 {
        my $self = $_[0];

        # Normalize the sigil here into $
        # $x    => $x
        # @x    => $List_x
        # %x    => $Hash_x
        # &x    => $Code_x
        my $table = {
            '$' => '$',
            '@' => '$List_',
            '%' => '$Hash_',
            '&' => '$Code_',
        };
        $table->{$.sigil} . $.name
    }
}

class Rul::Constant {
    has $.constant;
    sub emit_perl5 {
        my $self = $_[0];

        my $str = $.constant;
        Rul::constant( $str );
    }
    sub set_captures_to_array {
        my $self = $_[0];
 }
}

class Rul::Dot {
    sub emit_perl5 {
        my $self = $_[0];

        '( \'\' ne substr( $str, $MATCH->to, 1 ) ' .
        '&& ($MATCH->to = 1 + $MATCH->to)' .
        ')';
    }
    sub set_captures_to_array {
        my $self = $_[0];
 }
}

class Rul::SpecialChar {
    has $.char;
    sub emit_perl5 {
        my $self = $_[0];

        my $char = $.char;
        if ($char eq 'n') {
            return Rul::Subrule->new( metasyntax => 'is_newline', captures => 0 )->emit_perl5;
        }
        if ($char eq 'N') {
            return Rul::Subrule->new( metasyntax => 'not_newline', captures => 0 )->emit_perl5;
        }
        if ($char eq 'd') {
            return Rul::Subrule->new( metasyntax => 'digit', captures => 0 )->emit_perl5;
        }
        if ($char eq 's') {
            return Rul::Subrule->new( metasyntax => 'space', captures => 0 )->emit_perl5;
        }
        if ($char eq 't') {
            return Rul::constant( chr(9) );
        }
        return Rul::constant( $char );
    }
    sub set_captures_to_array {
        my $self = $_[0];
 }
}

class Rul::Block {
    has $.closure;
    sub emit_perl5 {
        my $self = $_[0];

        '(do { ' . $.closure . '; 1 })'
    }
    sub set_captures_to_array {
        my $self = $_[0];
 }
}

class Rul::InterpolateVar {
    has $.var;
    sub emit_perl5 {
        my $self = $_[0];

        say '# TODO: interpolate var ' . $.var->emit_perl5() . '';
        die();
    };
    sub set_captures_to_array {
        my $self = $_[0];
 }
}

class Rul::NamedCapture {
    has $.rule_exp;
    has $.capture_ident;
    sub emit_perl5 {
        my $self = $_[0];

        say '# TODO: named capture ' . $.capture_ident . ' = ' . $.rule_exp->emit_perl5() . '';
        die();
    }
    sub set_captures_to_array {
        my $self = $_[0];

        say '# TODO: named capture ';
    }
}

class Rul::Before {
    has $.rule_exp;
    sub emit_perl5 {
        my $self = $_[0];

        '(do { ' .
            'my $tmp = $MATCH; ' .
            '$MATCH = Perlito5::Match->new( \'str\' => $str, \'from\' => $tmp->to, \'to\' => $tmp->to, \'bool\' => 1  ); ' .
            '$MATCH->bool = ' .
                $.rule_exp->emit_perl5() .
            '; ' .
            '$tmp->bool = $MATCH->bool ? 1 : 0; ' .
            '$MATCH = $tmp; ' .
            '$MATCH->bool ? 1 : 0; ' .
        '})'
    }
    sub set_captures_to_array {
        my $self = $_[0];
 }
}

class Rul::NotBefore {
    has $.rule_exp;
    sub emit_perl5 {
        my $self = $_[0];

        '(do { ' .
            'my $tmp = $MATCH; ' .
            '$MATCH = Perlito5::Match->new( \'str\' => $str, \'from\' => $tmp->to, \'to\' => $tmp->to, \'bool\' => 1  ); ' .
            '$MATCH->bool = ' .
                $.rule_exp->emit_perl5() .
            '; ' .
            '$tmp->bool = !$MATCH->bool; ' .
            '$MATCH = $tmp; ' .
            '$MATCH->bool ? 1 : 0; ' .
        '})'
    }
    sub set_captures_to_array {
        my $self = $_[0];
 }
}

=begin

=head1 NAME

Perlito5::Emitter::Token - Code generator for Perlito Regex

=head1 SYNOPSIS

    my $match = $source.rule;
    $match->flat()->emit_perl5;    # generated Perlito source code

=head1 DESCRIPTION

This module generates Perlito code for the Regex compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
