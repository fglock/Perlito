use v5;


package Perlito5::Rul;
sub new { my $class = shift; bless {@_}, $class }

sub constant {
    my $str = shift;
    my $len = length $str;
    if ( $len ) {
        my @cond;
        my $i = 0;
        for my $char (split(//, $str)) {
            if ($char eq '\\') {
                $char = '\\\\';
            }
            if ($char eq '\'') {
                $char = '\\\'';
            }
            push @cond, '(\'' . $char . '\' eq $str->[$MATCH->{to} + ' . $i . '])';
            $i++;
        }
        return '('
        . join ( ' && ',
                 @cond,
                 '($MATCH->{to} += ' . $len . ')'
          )
        . ')';
    }
    else {
        return '1'
    }
}



package Perlito5::Rul::Quantifier;
sub new { my $class = shift; bless {@_}, $class }
sub term { $_[0]->{term} }
sub quant { $_[0]->{quant} }
sub emit_perl5 {
    my $self = $_[0];

    if ($self->{quant} eq '') {
        return $self->{term}->emit_perl5;
    }
    if ($self->{quant} eq '+') {
        $self->{term}->set_captures_to_array;
        return
            '(do { '
            .   'my $last_match_null = 0; '
            .   'my $m = $MATCH; '
            .   'my $to = $MATCH->{to}; '
            .   'my $count = 0; '
            .   'while (' . $self->{term}->emit_perl5() . ' && ($last_match_null < 2)) '
            .   '{ '
            .       'if ($to == $MATCH->{to}) { '
            .           '$last_match_null = $last_match_null + 1; '
            .       '} '
            .       'else { '
            .           '$last_match_null = 0; '
            .       '}; '
            .       '$m = $MATCH; '
            .       '$to = $MATCH->{to}; '
            .       '$count = $count + 1; '
            .   '}; '
            .   '$MATCH = $m; '
            .   '$MATCH->{to} = $to; '
            .   '$count > 0; '
            . '})';
    }
    if ($self->{quant} eq '*') {
        $self->{term}->set_captures_to_array;
        return
            '(do { '
            .   'my $last_match_null = 0; '
            .   'my $m = $MATCH; '
            .   'my $to = $MATCH->{to}; '
            .   'while (' . $self->{term}->emit_perl5() . ' && ($last_match_null < 2)) '
            .   '{ '
            .       'if ($to == $MATCH->{to}) { '
            .           '$last_match_null = $last_match_null + 1; '
            .       '} '
            .       'else { '
            .           '$last_match_null = 0; '
            .       '}; '
            .       '$m = $MATCH; '
            .       '$to = $MATCH->{to}; '
            .   '}; '
            .   '$MATCH = $m; '
            .   '$MATCH->{to} = $to; '
            .   '1 '
            . '})';
    }
    if ($self->{quant} eq '?') {
        $self->{term}->set_captures_to_array;
        return
            '(do { '
            .   'my $m = $MATCH; '
            .   'if (!' . $self->{term}->emit_perl5() . ') '
            .   '{ '
            .       '$MATCH = $m; '
            .   '}; '
            .   '1 '
            . '})';
    }

    die "Perlito5::Rul::Quantifier:  not implemented";
}
sub set_captures_to_array {
    my $self = $_[0];

    $self->{term}->set_captures_to_array;
}



package Perlito5::Rul::Or;
sub new { my $class = shift; bless {@_}, $class }
sub or_list { $_[0]->{or_list} }
sub emit_perl5 {
    my $self = $_[0];

    if ( scalar( @{$self->{or_list}} ) == 1 ) {
        return $self->{or_list}[0]->emit_perl5;
    }

    my $pos = '$' . Perlito5::get_label();

    '(do { '
        . 'my ' . $pos . ' = $MATCH->{to}; ('
        . join( ') || ($MATCH->{to} = ' . $pos . ', ',
              map( $_->emit_perl5, @{$self->{or_list}} )
            )
    . ') })';
}
sub set_captures_to_array {
    my $self = $_[0];

    map( $_->set_captures_to_array, @{$self->{or_list}} );
}



package Perlito5::Rul::Concat;
sub new { my $class = shift; bless {@_}, $class }
sub concat { $_[0]->{concat} }
sub emit_perl5 {
    my $self = $_[0];

    if ( scalar( @{$self->{concat}} ) == 1 ) {
        return $self->{concat}[0]->emit_perl5;
    }

    '('
        . join( ' && ',
                map( $_->emit_perl5, @{$self->{concat}} )
              )
    . ')';
}
sub set_captures_to_array {
    my $self = $_[0];

    map( $_->set_captures_to_array, @{$self->{concat}} );
}



package Perlito5::Rul::Subrule;
sub new { my $class = shift; bless {@_}, $class }
sub metasyntax { $_[0]->{metasyntax} }
sub captures { $_[0]->{captures} }
sub emit_perl5 {
    my $self = $_[0];
    my $meth = $self->{metasyntax};
    my $code;
    if ($self->{captures} == 1) {
        $code = 'if ($m2) { $MATCH->{to} = $m2->{to}; $MATCH->{\'' . $self->{metasyntax} . '\'} = $m2; 1 } else { 0 }; '
    }
    elsif ($self->{captures} > 1) {
        $code = 'if ($m2) { '
                .   '$MATCH->{to} = $m2->{to}; '
                .   'push @{ $MATCH->{\'' . $self->{metasyntax} . '\'} }, $m2; '
                .   '1 '
                . '} else { 0 }; '
    }
    else {
        $code = 'if ($m2) { $MATCH->{to} = $m2->{to}; 1 } else { 0 }; '
    }

    '(do { '
    .   'my $m2 = ' . $meth . '($str, $MATCH->{to}); '
    .   $code
    . '})'
}
sub set_captures_to_array {
    my $self = $_[0];

    if ($self->{captures} > 0) {
        $self->{captures} = $self->{captures} + 1;
    }
}



package Perlito5::Rul::Constant;
sub new { my $class = shift; bless {@_}, $class }
sub constant { $_[0]->{constant} }
sub emit_perl5 {
    my $self = $_[0];

    my $str = $self->{constant};
    Perlito5::Rul::constant( $str );
}
sub set_captures_to_array {
    my $self = $_[0];
}



package Perlito5::Rul::Dot;
sub new { my $class = shift; bless {@_}, $class }
sub emit_perl5 {
    my $self = $_[0];

    '(\'\' ne $str->[$MATCH->{to}] ' .
    '&& ++$MATCH->{to}' .
    ')';
}
sub set_captures_to_array {
    my $self = $_[0];
}



package Perlito5::Rul::SpecialChar;
sub new { my $class = shift; bless {@_}, $class }
sub char { $_[0]->{char} }
sub emit_perl5 {
    my $self = $_[0];

    my $char = $self->{char};
    if ($char eq 'n') {
        return Perlito5::Rul::Subrule->new( metasyntax => 'is_newline', captures => 0 )->emit_perl5;
    }
    if ($char eq 'N') {
        return Perlito5::Rul::Subrule->new( metasyntax => 'not_newline', captures => 0 )->emit_perl5;
    }
    if ($char eq 'd') {
        return Perlito5::Rul::Subrule->new( metasyntax => 'digit', captures => 0 )->emit_perl5;
    }
    if ($char eq 's') {
        return Perlito5::Rul::Subrule->new( metasyntax => 'space', captures => 0 )->emit_perl5;
    }
    if ($char eq 't') {
        return Perlito5::Rul::constant( chr(9) );
    }
    return Perlito5::Rul::constant( $char );
}
sub set_captures_to_array {
    my $self = $_[0];
}



package Perlito5::Rul::Block;
sub new { my $class = shift; bless {@_}, $class }
sub closure { $_[0]->{closure} }
sub emit_perl5 {
    my $self = $_[0];

    '(do { ' 
        . $self->{closure} 
        . '; 1 })'
}
sub set_captures_to_array {
    my $self = $_[0];
}



package Perlito5::Rul::Before;
sub new { my $class = shift; bless {@_}, $class }
sub rule_exp { $_[0]->{rule_exp} }
sub emit_perl5 {
    my $self = $_[0];

    '(do { ' .
        'my $tmp = $MATCH; ' .
        '$MATCH = { \'from\' => $tmp->{to}, \'to\' => $tmp->{to} }; ' .
        'my $res = ' .
            $self->{rule_exp}->emit_perl5() .
        '; ' .
        '$MATCH = $tmp; ' .
        '$res ? 1 : 0 ' .
    '})'
}
sub set_captures_to_array {
    my $self = $_[0];
}



package Perlito5::Rul::NotBefore;
sub new { my $class = shift; bless {@_}, $class }
sub rule_exp { $_[0]->{rule_exp} }
sub emit_perl5 {
    my $self = $_[0];

    '(do { ' .
        'my $tmp = $MATCH; ' .
        '$MATCH = { \'from\' => $tmp->{to}, \'to\' => $tmp->{to} }; ' .
        'my $res = ' .
            $self->{rule_exp}->emit_perl5() .
        '; ' .
        '$MATCH = $tmp; ' .
        '$res ? 0 : 1 ' .
    '})'
}
sub set_captures_to_array {
    my $self = $_[0];
}

1;

=begin

=head1 NAME

Perlito5::Emitter::Token - Code generator for Perlito Perl 5 grammar

=head1 SYNOPSIS

    my $match = $source.rule;
    Perlito5::Match::flat($match)->emit_perl5;    # generated Perlito source code

=head1 DESCRIPTION

This module generates Perl 5 code for the Regex6 compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011, 2012 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
