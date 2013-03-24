use strict;
use warnings;

package Perlito5::X64::Assembler;

my @buffer;
my @hex_char = qw( 0 1 2 3 4 5 6 7 8 9 A B C D E F );

sub new {
    my $class = shift;
    @buffer = ();
    bless {@_}, $class;
}

sub to_hex {
    return join(' ',
               map( $hex_char[int($_ / 16)] . $hex_char[$_ % 16], @buffer ) );
}

sub emit {
    push @buffer, $_[0];
}


sub is_zero { 
    return $_[0] == 0;
}

sub is_int8 {
    return -128 <= $_[0] && $_[0] < 128;
}

sub is_int16 {
    return -32768 <= $_[0] && $_[0] < 32768;
}

sub is_uint8 {
    return 0 <= $_[0] && $_[0] < 256;
}

sub is_uint16 {
    return 0 <= $_[0] && $_[0] < 65536;
}


#---

sub nop {
    emit(0x90);
}

sub ret {
    my ( $imm16 ) = @_;
    if ( !$imm16 ) {
        emit(0xC3);
    }
    else {
        emit(0xC2);
        emit( $imm16 & 0xFF );
        emit( ( $imm16 >> 8 ) & 0xFF );
    }
}

1;

__END__

=pod

=head1 Perlito5::X64::Assembler

The Perlito5 x64 backend

=head1 Synopsis

    use Perlito5::X64::Assembler;
    my $asm = Perlito6::X64::Assembler->new();
    say $asm->to_hex();

=head1 References

- V8 Javascript Compiler

    src/x64/assembler-x64.cc

=cut


