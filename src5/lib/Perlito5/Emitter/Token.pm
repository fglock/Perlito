use v5;


package Rul;
sub new { my $class = shift; bless {@_}, $class }

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
        '( \'' . $str . '\' eq substr( $str, $MATCH->{"to"}, ' . $len . ') ' .
        '&& ( $MATCH->{"to"} = ' . $len . ' + $MATCH->{"to"} )' .
        ')';
    }
    else {
        return '1'
    }
}



package Rul::Quantifier;
sub new { my $class = shift; bless {@_}, $class }
sub term { $_[0]->{'term'} }
sub quant { $_[0]->{'quant'} }
sub greedy { $_[0]->{'greedy'} }
sub ws1 { $_[0]->{'ws1'} }
sub ws2 { $_[0]->{'ws2'} }
sub ws3 { $_[0]->{'ws3'} }
sub emit_perl5 {
    my $self = $_[0];

    if ($self->{"quant"} eq '' && $self->{"greedy"} eq '') {
        return $self->{"term"}->emit_perl5;
    }
    if ($self->{"quant"} eq '+' && $self->{"greedy"} eq '') {
        $self->{"term"}->set_captures_to_array;
        return
            '(do { '
            .   'my $last_match_null = 0; '
            .   'my $last_pos = $MATCH->{"to"}; '
            .   'my $count = 0; '
            .   'while (' . $self->{"term"}->emit_perl5() . ' && ($last_match_null < 2)) '
            .   '{ '
            .       'if ($last_pos == $MATCH->{"to"}) { '
            .           '$last_match_null = $last_match_null + 1; '
            .       '} '
            .       'else { '
            .           '$last_match_null = 0; '
            .       '}; '
            .       '$last_pos = $MATCH->{"to"}; '
            .       '$count = $count + 1; '
            .   '}; '
            .   '$MATCH->{"to"} = $last_pos; '
            .   '$count > 0; '
            . '})';
    }
    if ($self->{"quant"} eq '*' && $self->{"greedy"} eq '') {
        $self->{"term"}->set_captures_to_array;
        return
            '(do { '
            .   'my $last_match_null = 0; '
            .   'my $last_pos = $MATCH->{"to"}; '
            .   'while (' . $self->{"term"}->emit_perl5() . ' && ($last_match_null < 2)) '
            .   '{ '
            .       'if ($last_pos == $MATCH->{"to"}) { '
            .           '$last_match_null = $last_match_null + 1; '
            .       '} '
            .       'else { '
            .           '$last_match_null = 0; '
            .       '}; '
            .       '$last_pos = $MATCH->{"to"}; '
            .   '}; '
            .   '$MATCH->{"to"} = $last_pos; '
            .   '1 '
            . '})';
    }
    if ($self->{"quant"} eq '?' && $self->{"greedy"} eq '') {
        $self->{"term"}->set_captures_to_array;
        return
            '(do { '
            .   'my $last_pos = $MATCH->{"to"}; '
            .   'if (!(do {' . $self->{"term"}->emit_perl5() . '})) '
            .   '{ '
            .       '$MATCH->{"to"} = $last_pos; '
            .   '}; '
            .   '1 '
            . '})';
    }

    # TODO
    warn "Rul::Quantifier:  not implemented";
    $self->{"term"}->emit_perl5;
}
sub set_captures_to_array {
    my $self = $_[0];

    $self->{"term"}->set_captures_to_array;
}



package Rul::Or;
sub new { my $class = shift; bless {@_}, $class }
sub or_list { $_[0]->{'or_list'} }
sub emit_perl5 {
    my $self = $_[0];

    '(do { '
        . 'my $pos1 = $MATCH->{"to"}; (do { '
        . join( '}) || (do { $MATCH->{"to"} = $pos1; ',
              map( $_->emit_perl5, @{$self->{"or_list"}} )
            )
    . '}) })';
}
sub set_captures_to_array {
    my $self = $_[0];

    map( $_->set_captures_to_array, @{$self->{"or_list"}} );
}



package Rul::Concat;
sub new { my $class = shift; bless {@_}, $class }
sub concat { $_[0]->{'concat'} }
sub emit_perl5 {
    my $self = $_[0];

    '('
        . join( ' && ',
                map( $_->emit_perl5, @{$self->{"concat"}} )
              )
    . ')';
}
sub set_captures_to_array {
    my $self = $_[0];

    map( $_->set_captures_to_array, @{$self->{"concat"}} );
}



package Rul::Perlito5::AST::Subrule;
sub new { my $class = shift; bless {@_}, $class }
sub metasyntax { $_[0]->{'metasyntax'} }
sub captures { $_[0]->{'captures'} }
sub emit_perl5 {
    my $self = $_[0];

    my $meth = ( 1 + index( $self->{"metasyntax"}, '.' ) )
        ? Perlito5::Runtime::_replace( $self->{"metasyntax"}, '.', '->' )
        : ( '$grammar->' . $self->{"metasyntax"} );

    my $code;
    if ($self->{"captures"} == 1) {
        $code = 'if ($m2->{"bool"}) { $MATCH->{"to"} = $m2->{"to"}; $MATCH->{\'' . $self->{"metasyntax"} . '\'} = $m2; 1 } else { 0 }; '
    }
    elsif ($self->{"captures"} > 1) {
        # TODO: capture level > 2
        $code = 'if ($m2->{"bool"}) { '
                .   '$MATCH->{"to"} = $m2->{"to"}; '
                .   'if (exists $MATCH->{\'' . $self->{"metasyntax"} . '\'}) { '
                .       'push @{ $MATCH->{\'' . $self->{"metasyntax"} . '\'} }, $m2; '
                .   '} '
                .   'else { '
                .       '$MATCH->{\'' . $self->{"metasyntax"} . '\'} = [ $m2 ]; '
                .   '}; '
                .   '1 '
                . '} else { 0 }; '
    }
    else {
        $code = 'if ($m2->{"bool"}) { $MATCH->{"to"} = $m2->{"to"}; 1 } else { 0 }; '
    }

    '(do { '
    .   'my $m2 = ' . $meth . '($str, $MATCH->{"to"}); '
    .   $code
    . '})'
}
sub set_captures_to_array {
    my $self = $_[0];

    if ($self->{"captures"} > 0) {
        $self->{"captures"} = $self->{"captures"} + 1;
    }
}



package Rul::Constant;
sub new { my $class = shift; bless {@_}, $class }
sub constant { $_[0]->{'constant'} }
sub emit_perl5 {
    my $self = $_[0];

    my $str = $self->{"constant"};
    Rul::constant( $str );
}
sub set_captures_to_array {
    my $self = $_[0];
}



package Rul::Perlito5::AST::Dot;
sub new { my $class = shift; bless {@_}, $class }
sub emit_perl5 {
    my $self = $_[0];

    '( \'\' ne substr( $str, $MATCH->{"to"}, 1 ) ' .
    '&& ($MATCH->{"to"} = 1 + $MATCH->{"to"})' .
    ')';
}
sub set_captures_to_array {
    my $self = $_[0];
}



package Rul::SpecialChar;
sub new { my $class = shift; bless {@_}, $class }
sub char { $_[0]->{'char'} }
sub emit_perl5 {
    my $self = $_[0];

    my $char = $self->{"char"};
    if ($char eq 'n') {
        return Rul::Perlito5::AST::Subrule->new( metasyntax => 'is_newline', captures => 0 )->emit_perl5;
    }
    if ($char eq 'N') {
        return Rul::Perlito5::AST::Subrule->new( metasyntax => 'not_newline', captures => 0 )->emit_perl5;
    }
    if ($char eq 'd') {
        return Rul::Perlito5::AST::Subrule->new( metasyntax => 'digit', captures => 0 )->emit_perl5;
    }
    if ($char eq 's') {
        return Rul::Perlito5::AST::Subrule->new( metasyntax => 'space', captures => 0 )->emit_perl5;
    }
    if ($char eq 't') {
        return Rul::constant( chr(9) );
    }
    return Rul::constant( $char );
}
sub set_captures_to_array {
    my $self = $_[0];
}



package Rul::Block;
sub new { my $class = shift; bless {@_}, $class }
sub closure { $_[0]->{'closure'} }
sub emit_perl5 {
    my $self = $_[0];

    '(do { ' . $self->{"closure"} . '; 1 })'
}
sub set_captures_to_array {
    my $self = $_[0];
}



package Rul::Before;
sub new { my $class = shift; bless {@_}, $class }
sub rule_exp { $_[0]->{'rule_exp'} }
sub emit_perl5 {
    my $self = $_[0];

    '(do { ' .
        'my $tmp = $MATCH; ' .
        '$MATCH = Perlito5::Match->new( \'str\' => $str, \'from\' => $tmp->{"to"}, \'to\' => $tmp->{"to"}, \'bool\' => 1  ); ' .
        '$MATCH->{"bool"} = ' .
            $self->{"rule_exp"}->emit_perl5() .
        '; ' .
        '$tmp->{"bool"} = $MATCH->{"bool"} ? 1 : 0; ' .
        '$MATCH = $tmp; ' .
        '$MATCH->{"bool"} ? 1 : 0; ' .
    '})'
}
sub set_captures_to_array {
    my $self = $_[0];
}



package Rul::NotBefore;
sub new { my $class = shift; bless {@_}, $class }
sub rule_exp { $_[0]->{'rule_exp'} }
sub emit_perl5 {
    my $self = $_[0];

    '(do { ' .
        'my $tmp = $MATCH; ' .
        '$MATCH = Perlito5::Match->new( \'str\' => $str, \'from\' => $tmp->{"to"}, \'to\' => $tmp->{"to"}, \'bool\' => 1  ); ' .
        '$MATCH->{"bool"} = ' .
            $self->{"rule_exp"}->emit_perl5() .
        '; ' .
        '$tmp->{"bool"} = !$MATCH->{"bool"}; ' .
        '$MATCH = $tmp; ' .
        '$MATCH->{"bool"} ? 1 : 0; ' .
    '})'
}
sub set_captures_to_array {
    my $self = $_[0];
}


=begin

=head1 NAME

Perlito5::Emitter::Token - Code generator for Perlito Perl 5 grammar

=head1 SYNOPSIS

    my $match = $source.rule;
    $match->flat()->emit_perl5;    # generated Perlito source code

=head1 DESCRIPTION

This module generates Perl 5 code for the Regex compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011, 2012 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
