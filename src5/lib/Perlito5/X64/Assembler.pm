use strict;
use warnings;

package Perlito6::X64::Assembler;

sub new {
    my $class = shift;
    bless {@_}, $class;
}

sub to_hex {
    my $self = $_[0];
    my $text = $_[1];
    return '';
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

=cut


