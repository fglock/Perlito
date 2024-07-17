# Argument types --------

## sub parse_arg_list {
##     my ( $tokens, $index ) = @_;
##     return parse_precedence_expression( $tokens, $index, $LIST_OPERATOR_PRECEDENCE );
## }
sub parse_arg_list {
    my ( $tokens, $index ) = @_;
    my $pos = $index;
    my $ast;
    my @expr;
  LIST:
    while (1) {
        $pos = parse_optional_whitespace( $tokens, $pos );
        last LIST if $tokens->[$pos][0] == END_TOKEN();
        if ( $tokens->[$pos][0] == COMMA() || $tokens->[$pos][0] == FAT_ARROW() ) {
            $pos++;
            next LIST;
        }
        $ast = parse_precedence_expression( $tokens, $pos, $PRECEDENCE{','} + 1 );
        last LIST if $ast->{FAIL};
        $pos = $ast->{next};
        push @expr, $ast;
        $pos = parse_optional_whitespace( $tokens, $pos );
        if ( $tokens->[$pos][0] != COMMA() && $tokens->[$pos][0] != FAT_ARROW() ) {
            last LIST;
        }
    }
    return { %{ $expr[0] }, next => $pos } if @expr == 1;
    return { type => 'LIST_OP', index => $index, value => \@expr, next => $pos };
}

sub parse_single_arg {
    my ( $tokens, $index ) = @_;
    return parse_fail() if $tokens->[$index][0] == COMMA();
    return parse_precedence_expression( $tokens, $index, $PRECEDENCE{','} + 1 );
}

sub parse_single_arg_array {
    my ( $tokens, $index ) = @_;
    my $ast = parse_single_arg( $tokens, $index );
    return $ast         if $ast->{FAIL};
    return parse_fail() if $ast->{type} eq 'PREFIX_OP' && $ast->{value}{op} ne '@';
    return $ast;
}

#----------------------

