package strict;

# magic numbers - See lib/strict.pm in perl distribution
$Perlito5::STRICT_REFS = 0x00000002;
$Perlito5::STRICT_SUBS = 0x00000200;
$Perlito5::STRICT_VARS = 0x00000400;

sub import {
    my ($pkg, @args) = @_;
    @args = ( 'refs', 'subs', 'vars' )
        if !@args;
    for (@args) {
        $^H |= $Perlito5::STRICT_REFS if $_ eq 'refs';
        $^H |= $Perlito5::STRICT_SUBS if $_ eq 'subs';
        $^H |= $Perlito5::STRICT_VARS if $_ eq 'vars';
    }
}

sub unimport {
    my ($pkg, @args) = @_;
    @args = ( 'refs', 'subs', 'vars' )
        if !@args;
    for (@args) {
        $^H &= ~$Perlito5::STRICT_REFS if $_ eq 'refs';
        $^H &= ~$Perlito5::STRICT_SUBS if $_ eq 'subs';
        $^H &= ~$Perlito5::STRICT_VARS if $_ eq 'vars';
    }
}

1;

