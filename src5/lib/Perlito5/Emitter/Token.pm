use v6;

# no <after .. > - so it doesn't need to match backwards
# no backtracking on quantifiers

class Rul {

    sub constant {
            my $str = shift;
            my $len = $str->chars;
            if ($str eq '\\') {
                $str = '\\\\';
            }
            if ($str eq '\'') {
                $str = '\\\'';
            }
            if ( $len ) {
                '( \'' . $str . '\' eq substr( $str, $MATCH->to, ' . $len . ') ' ~
                '&& ( $MATCH->to = ' . $len . ' + $MATCH->to )' ~
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
    method emit_perl6 {
        if (($.quant eq '') && ($.greedy eq '')) {
            return $.term->emit_perl6;
        }
        if (($.quant eq '+') && ($.greedy eq '')) {
            $.term->set_captures_to_array;
            return
                '(do { '
                .   'my $last_match_null = 0; '
                .   'my $last_pos = $MATCH->to; '
                .   'my $count = 0; '
                .   'while (' . $.term->emit_perl6() . ' && ($last_match_null < 2)) '
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
                .   'while (' . $.term->emit_perl6() . ' && ($last_match_null < 2)) '
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
                .   'if (!(do {' . $.term->emit_perl6() . '})) '
                .   '{ '
                .       '$MATCH->to = $last_pos; '
                .   '}; '
                .   '1 '
                . '})';
        }

        # TODO
        warn "Rul::Quantifier: " . self->perl . " not implemented";
        $.term->emit_perl6;
    }
    method set_captures_to_array {
        $.term->set_captures_to_array;
    }
}

class Rul::Or {
    has @.or_list;
    method emit_perl6 {
        '(do { ' ~
            'my $pos1 = $MATCH->to; (do { ' ~
            (@.or_list.>>emit_perl6)->join('}) || (do { $MATCH->to = $pos1; ') ~
        '}) })';
    }
    method set_captures_to_array {
        @.or_list.>>set_captures_to_array;
    }
}

class Rul::Concat {
    has @.concat;
    method emit_perl6 {
        '(' . (@.concat.>>emit_perl6)->join(' && ') . ')';
    }
    method set_captures_to_array {
        @.concat.>>set_captures_to_array;
    }
}

class Rul::Subrule {
    has $.metasyntax;
    has $.captures;
    method emit_perl6 {
        my $meth = ( 1 + index( $.metasyntax, '.' ) )
            ? Main::_replace( $.metasyntax, '.', '->' )
            : ( '$grammar->' . $.metasyntax );

        my $code;
        if ($.captures == 1) {
            $code = 'if ($m2) { $MATCH->to = $m2->to; $MATCH->{\'' . $.metasyntax . '\'} = $m2; 1 } else { 0 }; '
        }
        elsif $.captures > 1 {
            # TODO: capture level > 2
            $code = 'if ($m2) { '
                    .   '$MATCH->to = $m2->to; '
                    .   'if (exists $MATCH->{\'' . $.metasyntax . '\'}) { '
                    .       '($MATCH->{\'' . $.metasyntax . '\'})->push( $m2 ); '
                    .   '} '
                    .   'else { '
                    .       '$MATCH->{\'' . $.metasyntax . '\'} = [ $m2 ]; '
                    .   '}; '
                    .   '1 '
                    . '} else { 0 }; '
        }
        else {
            $code = 'if ($m2) { $MATCH->to = $m2->to; 1 } else { 0 }; '
        }

        '(do { '
        .   'my $m2 = ' . $meth . '($str, $MATCH->to); '
        .   $code
        . '})'
    }
    method set_captures_to_array {
        if ($.captures > 0) {
            $.captures = $.captures + 1;
        }
    }
}

class Rul::Var {
    has $.sigil;
    has $.twigil;
    has $.name;
    method emit_perl6 {
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
    method emit_perl6 {
        my $str = $.constant;
        Rul::constant( $str );
    }
    method set_captures_to_array { }
}

class Rul::Dot {
    method emit_perl6 {
        '( \'\' ne substr( $str, $MATCH->to, 1 ) ' ~
        '&& ($MATCH->to = 1 + $MATCH->to)' ~
        ')';
    }
    method set_captures_to_array { }
}

class Rul::SpecialChar {
    has $.char;
    method emit_perl6 {
        my $char = $.char;
        if ($char eq 'n') {
            return Rul::Subrule->new( metasyntax => 'is_newline', captures => 0 )->emit_perl6;
        }
        if ($char eq 'N') {
            return Rul::Subrule->new( metasyntax => 'not_newline', captures => 0 )->emit_perl6;
        }
        if ($char eq 'd') {
            return Rul::Subrule->new( metasyntax => 'digit', captures => 0 )->emit_perl6;
        }
        if ($char eq 's') {
            return Rul::Subrule->new( metasyntax => 'space', captures => 0 )->emit_perl6;
        }
        if ($char eq 't') {
            return Rul::constant( chr(9) );
        }
        return Rul::constant( $char );
    }
    method set_captures_to_array { }
}

class Rul::Block {
    has $.closure;
    method emit_perl6 {
        '((do { ' . $.closure . ' }) || 1)'
    }
    method set_captures_to_array { }
}

class Rul::InterpolateVar {
    has $.var;
    method emit_perl6 {
        say '# TODO: interpolate var ' . $.var->emit_perl6() . '';
        die();
    };
    method set_captures_to_array { }
}

class Rul::NamedCapture {
    has $.rule_exp;
    has $.capture_ident;
    method emit_perl6 {
        say '# TODO: named capture ' . $.capture_ident . ' = ' . $.rule_exp->emit_perl6() . '';
        die();
    }
    method set_captures_to_array {
        say '# TODO: named capture ';
    }
}

class Rul::Before {
    has $.rule_exp;
    method emit_perl6 {
        '(do { ' ~
            'my $tmp = $MATCH; ' ~
            '$MATCH = Perlito5::Match->new( \'str\' => $str, \'from\' => $tmp->to, \'to\' => $tmp->to, \'bool\' => 1  ); ' ~
            '$MATCH->bool = ' ~
                $.rule_exp->emit_perl6() ~
            '; ' ~
            '$tmp->bool = $MATCH ? 1 : 0; ' ~
            '$MATCH = $tmp; ' ~
            '$MATCH ? 1 : 0; ' ~
        '})'
    }
    method set_captures_to_array { }
}

class Rul::NotBefore {
    has $.rule_exp;
    method emit_perl6 {
        '(do { ' ~
            'my $tmp = $MATCH; ' ~
            '$MATCH = Perlito5::Match->new( \'str\' => $str, \'from\' => $tmp->to, \'to\' => $tmp->to, \'bool\' => 1  ); ' ~
            '$MATCH->bool = ' ~
                $.rule_exp->emit_perl6() ~
            '; ' ~
            '$tmp->bool = !$MATCH; ' ~
            '$MATCH = $tmp; ' ~
            '$MATCH ? 1 : 0; ' ~
        '})'
    }
    method set_captures_to_array { }
}

class Rul::NegateCharClass {
    has $.chars;
    method emit_perl6 {
        say "TODO NegateCharClass";
        die();
    }
}

class Rul::CharClass {
    has $.chars;
    method emit_perl6 {
        say "TODO CharClass";
        die();
    }
}

class Rul::Capture {
    has $.rule_exp;
    method emit_perl6 {
        say "TODO RulCapture";
        die();
    }
}

class Rul::CaptureResult {
    has $.rule_exp;
    method emit_perl6 {
        say "TODO Rul::CaptureResult";
        die();
    }
}

class Rul::After {
    has $.rule_exp;
    method emit_perl6 {
        say "TODO Rul::After";
        die();
    }
}

=begin

=head1 NAME

Perlito5::Emitter::Token - Code generator for Perlito Regex

=head1 SYNOPSIS

    my $match = $source.rule;
    ($$match)->emit_perl6;    # generated Perlito source code

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
