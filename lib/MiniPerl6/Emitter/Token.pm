use v6;

# no <after .. > - so it doesn't need to match backwards
# no backtracking on quantifiers

class Rul {
    
    sub constant ( $str ) {
            my $len := $str.chars;
            if $str eq '\\' {
                $str := '\\\\';
            };
            if $str eq '\'' {
                $str := '\\\'';
            };
            if ( $len ) {
                '( ( \'' ~ $str ~ '\' eq substr( $str, $MATCH.to, ' ~ $len ~ ')) ' ~
                '  ?? (1 + ( $MATCH.to := ' ~ $len ~ ' + $MATCH.to ))' ~
                '  !! false ' ~
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
    method emit {
        # TODO
        $.term.emit;
    }
}

class Rul::Or {
    has @.or_list;
    method emit {
        'do { ' ~
            'my $pos1 := $MATCH.to; do{ ' ~ 
            (@.or_list.>>emit).join('} || do { $MATCH.to := $pos1; ') ~
        '} }';
    }
}

class Rul::Concat {
    has @.concat;
    method emit {
        '(' ~ (@.concat.>>emit).join(' && ') ~ ')';
    }
}

class Rul::Subrule {
    has $.metasyntax;
    method emit {
        my $meth := ( 1 + index( $.metasyntax, '.' ) )
            ?? $.metasyntax 
            !! ( '$grammar.' ~ $.metasyntax );
        'do { ' ~
          'my $m2 := ' ~ $meth ~ '($str, $MATCH.to); ' ~
          'if $m2 { $MATCH.to := $m2.to; $MATCH{\'' ~ $.metasyntax ~ '\'} := $m2; 1 } else { false } ' ~
        '}'
    }
}

class Rul::SubruleNoCapture {
    has $.metasyntax;
    method emit {
        my $meth := ( 1 + index( $.metasyntax, '.' ) )
            ?? $.metasyntax 
            !! ( '$grammar.' ~ $.metasyntax );
        'do { ' ~
          'my $m2 := ' ~ $meth ~ '($str, $MATCH.to); ' ~
          'if $m2 { $MATCH.to := $m2.to; 1 } else { false } ' ~
        '}'
    }
}

class Rul::Var {
    has $.sigil;
    has $.twigil;
    has $.name;
    method emit {
        # Normalize the sigil here into $
        # $x    => $x
        # @x    => $List_x
        # %x    => $Hash_x
        # &x    => $Code_x
        my $table := {
            '$' => '$',
            '@' => '$List_',
            '%' => '$Hash_',
            '&' => '$Code_',
        };
        $table{$.sigil} ~ $.name
    }
}

class Rul::Constant {
    has $.constant;
    method emit {
        my $str := $.constant; 
        Rul::constant( $str );
    }
}

class Rul::Dot {
    method emit {
        '( (\'\' ne substr( $str, $MATCH.to, 1 )) ' ~
        '  ?? (1 + ($MATCH.to := 1 + $MATCH.to ))' ~
        '  !! false ' ~
        ')';
    }
}

class Rul::SpecialChar {
    has $.char;
    method emit {
        my $char := $.char;
        if $char eq 'n' {
            my $rul := Rul::SubruleNoCapture.new( metasyntax => 'is_newline' );
            $rul := $rul.emit;
            return $rul;
        };
        if $char eq 'N' {
            my $rul := Rul::SubruleNoCapture.new( metasyntax => 'not_newline' );
            $rul := $rul.emit;
            return $rul;
        };
        if $char eq 'd' {
            my $rul := Rul::SubruleNoCapture.new( metasyntax => 'digit' );
            $rul := $rul.emit;
            return $rul;
        };
        if $char eq 's' {
            my $rul := Rul::SubruleNoCapture.new( metasyntax => 'space' );
            $rul := $rul.emit;
            return $rul;
        };
        return Rul::constant( $char );
    }
}

class Rul::Block {
    has $.closure;
    method emit {
        '(do { ' ~ $.closure ~ ' } || 1)'
    }
}

class Rul::InterpolateVar {
    has $.var;
    method emit {
        say '# TODO: interpolate var ' ~ $.var.emit ~ '';
        die();
    };
}

class Rul::NamedCapture {
    has $.rule_exp;
    has $.capture_ident;
    method emit {
        say '# TODO: named capture ' ~ $.capture_ident ~ ' := ' ~ $.rule_exp.emit ~ '';
        die();
    }
}

class Rul::Before {
    has $.rule_exp;
    method emit {
        'do { ' ~
            'my $tmp := $MATCH; ' ~
            '$MATCH := MiniPerl6::Match.new( \'str\' => $str, \'from\' => $tmp.to, \'to\' => $tmp.to, \'bool\' => 1  ); ' ~
            '$MATCH.bool := ' ~
                $.rule_exp.emit ~
            '; ' ~
            '$tmp.bool := ?$MATCH; ' ~
            '$MATCH := $tmp; ' ~
            '?$MATCH; ' ~
        '}'
    }
}

class Rul::NotBefore {
    has $.rule_exp;
    method emit {
        'do { ' ~
            'my $tmp := $MATCH; ' ~
            '$MATCH := MiniPerl6::Match.new( \'str\' => $str, \'from\' => $tmp.to, \'to\' => $tmp.to, \'bool\' => 1  ); ' ~
            '$MATCH.bool := ' ~
                $.rule_exp.emit ~
            '; ' ~
            '$tmp.bool := !$MATCH; ' ~
            '$MATCH := $tmp; ' ~
            '?$MATCH; ' ~
        '}'
    }
}

class Rul::NegateCharClass {
    has $.chars;
    method emit {
        say "TODO NegateCharClass";
        die();
    }
}

class Rul::CharClass {
    has $.chars;
    method emit {
        say "TODO CharClass";
        die();
    }
}

class Rul::Capture {
    has $.rule_exp;
    method emit {
        say "TODO RulCapture";
        die();
    }
}

class Rul::CaptureResult {
    has $.rule_exp;
    method emit {
        say "TODO Rul::CaptureResult";
        die();
    }
}

class Rul::After {
    has $.rule_exp;
    method emit {
        say "TODO Rul::After";
        die();
    }
}

=begin

=head1 NAME 

MiniPerl6::Emitter::Token - Code generator for MiniPerl6 Regex

=head1 SYNOPSIS

    my $match := $source.rule;
    ($$match).emit;    # generated MiniPerl6 source code

=head1 DESCRIPTION

This module generates MiniPerl6 code for the Regex compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006, 2009 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
