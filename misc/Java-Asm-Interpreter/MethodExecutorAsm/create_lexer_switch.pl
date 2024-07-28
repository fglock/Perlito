#!/usr/bin/perl
use strict;
use warnings;

# List of Perl operators with 2 or 3 letters
my @operators = qw(
    != !~ $# %= && &= ** *= ++ += -- -= -> .. .= // /= :: << <= == => =~ >= >> ^= ^^ x= |= || ~~
    &&= &.= **= ... //= <<= <=> >>= ^.= |.= ||= 
);

# Create a hash to store operators by their first character
my %operators_by_char;
foreach my $operator (@operators) {
    my $first_char = substr($operator, 0, 1);
    push @{$operators_by_char{$first_char}}, $operator;
}

# Generate the switch statement
print "private Token consumeOperator() {\n";
print "    switch (current) {\n";

foreach my $char (sort keys %operators_by_char) {
    print "        case '$char':\n";
    my @ops = @{$operators_by_char{$char}};
    foreach my $op (sort { length($b) <=> length($a) } @ops) {
        my $len = length($op);
        if ($len > 1) {
            my $check = join(' && ', map { "input[position + $_] == '" . substr($op, $_, 1) . "'" } 1..$len-1);
            print "            if (position + $len <= input.length && $check) {\n";
            print "                position += $len;\n";
            print "                return new Token(TokenType.OPERATOR, \"$op\");\n";
            print "            }\n";
        } else {
            print "            if (next == '" . substr($op, 1, 1) . "') {\n";
            print "                position++;\n";
            print "                return new Token(TokenType.OPERATOR, \"$op\");\n";
            print "            }\n";
        }
    }
    print "            break;\n";
}

print "    }\n";
print "    return null;\n";
print "}\n";

