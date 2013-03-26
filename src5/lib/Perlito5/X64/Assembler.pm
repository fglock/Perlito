use strict;
use warnings;


package Perlito5::X64::Label;

sub new {
    my $class = shift;
    bless {@_}, $class;
}

sub pos {
    return $_[0]->{pos} 
}

sub bind {
    $_[0]->{pos} = $_[1];
}

sub is_bound {
    return defined( $_[0]->{pos} );
}


package Perlito5::X64::Register;

sub new {
    my $class = shift;
    bless {@_}, $class;
}

sub code {
    return $_[0]->{code} 
}

sub is {
    return $_[0]->{code} == $_[1]->{code};
}

# Return the high bit of the register code as a 0 or 1.  Used often when constructing the REX prefix byte.
sub high_bit {
    my ($reg) = @_;
    return $reg->{code} >> 3;
}

# Return the 3 low bits of the register code.  Used when encoding registers in modR/M, SIB, and opcode bytes.
sub low_bits {
    my ($reg) = @_;
    return $reg->{code} & 0x7;
}

# emit REX if needed
sub emit_optional_rex_32 {
    my ($reg) = @_;
    Perlito5::X64::Assembler::emit(0x41) if $reg->high_bit();
}


package Perlito5::X64::Assembler;

my @buffer;
my @hex_char = qw( 0 1 2 3 4 5 6 7 8 9 A B C D E F );
my $predictable_code_size = 1;

#--- registers

my $r_rax = Perlito5::X64::Register->new( code => 0  );
my $r_rcx = Perlito5::X64::Register->new( code => 1  );
my $r_rdx = Perlito5::X64::Register->new( code => 2  );
my $r_rbx = Perlito5::X64::Register->new( code => 3  );
my $r_rsp = Perlito5::X64::Register->new( code => 4  );
my $r_rbp = Perlito5::X64::Register->new( code => 5  );
my $r_rsi = Perlito5::X64::Register->new( code => 6  );
my $r_rdi = Perlito5::X64::Register->new( code => 7  );
my $r_r8  = Perlito5::X64::Register->new( code => 8  );
my $r_r9  = Perlito5::X64::Register->new( code => 9  );
my $r_r10 = Perlito5::X64::Register->new( code => 10 );
my $r_r11 = Perlito5::X64::Register->new( code => 11 );
my $r_r12 = Perlito5::X64::Register->new( code => 12 );
my $r_r13 = Perlito5::X64::Register->new( code => 13 );
my $r_r14 = Perlito5::X64::Register->new( code => 14 );
my $r_r15 = Perlito5::X64::Register->new( code => 15 );

sub rax () { $r_rax } 
sub rcx () { $r_rcx }
sub rdx () { $r_rdx }
sub rbx () { $r_rbx }
sub rsp () { $r_rsp }
sub rbp () { $r_rbp }
sub rsi () { $r_rsi }
sub rdi () { $r_rdi }
sub r8  () { $r_r8  }
sub r9  () { $r_r9  }
sub r10 () { $r_r10 }
sub r11 () { $r_r11 }
sub r12 () { $r_r12 }
sub r13 () { $r_r13 }
sub r14 () { $r_r14 }
sub r15 () { $r_r15 }

#--- scale factors for operands

sub times_1 () { 0 }
sub times_2 () { 1 }
sub times_4 () { 2 }
sub times_8 () { 3 }
sub times_int_size     () { times_4 }
sub times_pointer_size () { times_8 }

#--- conditions

  # # any value < 0 is considered no_condition
  # no_condition  = -1,

sub overflow      () {  0 }
sub no_overflow   () {  1 }
sub below         () {  2 }
sub above_equal   () {  3 }
sub equal         () {  4 }
sub not_equal     () {  5 }
sub below_equal   () {  6 }
sub above         () {  7 }
sub negative      () {  8 }
sub positive      () {  9 }
sub parity_even   () { 10 }
sub parity_odd    () { 11 }
sub less          () { 12 }
sub greater_equal () { 13 }
sub less_equal    () { 14 }
sub greater       () { 15 }

# Fake conditions that are handled by the
# opcodes using them.
sub always        () { 16 }
sub never         () { 17 }

  # # aliases
  # carry         = below,
  # not_carry     = above_equal,
  # zero          = equal,
  # not_zero      = not_equal,
  # sign          = negative,
  # not_sign      = positive,
  # last_condition = greater

#--- general

sub new {
    my $class = shift;
    @buffer = ();
    bless {@_}, $class;
}

sub to_hex {
    return join(' ',
               map( $hex_char[int($_ / 16)] . $hex_char[$_ % 16], @buffer ) );
}

sub asm_reset {
    @buffer = ();
}

sub emit {
    push @buffer, $_[0];
}

sub emitw {
    my ($v) = @_;
    emit( $v & 0xFF );
    emit( ( $v >> 8 ) & 0xFF );
}

sub emitl {
    my ($v) = @_;
    emit( $v & 0xFF );
    emit( ( $v >> 8  ) & 0xFF );
    emit( ( $v >> 16 ) & 0xFF );
    emit( ( $v >> 24 ) & 0xFF );
}

sub emit_rex_64 {
    my ($reg, $rm_reg) = @_;
    if ( @_ == 0 ) {
        # Emit a REX prefix that only sets REX.W to choose a 64-bit operand size.
        emit(0x48);
    }
    elsif ( @_ == 1 ) {
        # Emits a REX prefix that encodes a 64-bit operand size and
        # the top bit of the register code.
        # The high bit of register is used for REX.B.
        # REX.W is set and REX.R and REX.X are clear.
        emit(0x48 | $reg->high_bit());
    }
    elsif ( is_register($reg) && is_register($rm_reg) ) {
        emit(0x48 | $reg->high_bit() << 2 | $rm_reg->high_bit());
    }
    else {
        die "emit_rex_64: don't know what to do with $reg, $rm_reg";
    }
}

sub emit_modrm {
    my ($reg, $rm_reg) = @_;
    if ( is_register($reg) && is_register($rm_reg) ) {
        # Emit a ModR/M byte with registers coded in the reg and rm_reg fields.
        emit(0xC0 | $reg->low_bits() << 3 | $rm_reg->low_bits());
    }
    elsif ( is_register($rm_reg) ) {
        # Emit a ModR/M byte with an operation subcode in the reg field and
        # a register in the rm_reg field.
        my ($code, $rm_reg) = @_;
        emit(0xC0 | $code << 3 | $rm_reg->low_bits());
    }
    else {
        die "emit_modrm: don't know what to do with $reg, $rm_reg";
    }
}

sub is_register {
    ref($_[0]) eq 'Perlito5::X64::Register'
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

sub is_uint4 {
    return 0 <= $_[0] && $_[0] < 16;
}

sub is_uint8 {
    return 0 <= $_[0] && $_[0] < 256;
}

sub is_uint16 {
    return 0 <= $_[0] && $_[0] < 65536;
}

sub label {
    return Perlito5::X64::Label->new();
}

sub pc_offset {
    return scalar( @buffer );
}

sub predictable_code_size {
    return $predictable_code_size;
}

#--- instructions

# bind the current address to a label
sub _bind {
    my ($label) = @_;
    die "bind: expecting a label"
        if ref($label) ne 'Perlito5::X64::Label';
    $label->bind( scalar(@buffer) );
}

sub _cpuid {
    emit(0x0F);
    emit(0xA2);
}

sub _cqo {
    emit_rex_64();
    emit(0x99);
}

sub _hlt {
    emit(0xF4);
}

sub _int3 {
    emit(0xCC);
}

sub _j {
    my ( $condition, $label, $distance ) = @_;

    if ($condition == always) {
        _jmp($label);
        return;
    } 
    elsif ($condition == never) {
        return;
    }
    die if !is_uint4($condition);

    if ( $label->is_bound() ) {
        my $short_size = 2;
        my $long_size  = 6;
        my $offs = $label->pos() - pc_offset();
        die if !($offs <= 0);
        # Determine whether we can use 1-byte offsets for backwards branches,
        # which have a max range of 128 bytes.

        # We also need to check predictable_code_size() flag here, because on x64,
        # when the full code generator recompiles code for debugging, some places
        # need to be padded out to a certain size. The debugger is keeping track of
        # how often it did this so that it can adjust return addresses on the
        # stack, but if the size of jump instructions can also change, that's not
        # enough and the calculated offsets would be incorrect.
        if ( is_int8( $offs - $short_size ) && !predictable_code_size() ) {
          # 0111 tttn #8-bit disp.
          emit(0x70 | $condition);
          emit(($offs - $short_size) & 0xFF);
        } else {
          # 0000 1111 1000 tttn #32-bit disp.
          emit(0x0F);
          emit(0x80 | $condition);
          emitl($offs - $long_size);
        }
    }
    else {
        die "j: don't know what to do with @_";
    }
}

sub _jmp {
    my ( $label, $distance ) = @_;

    my $short_size = 1;  # sizeof(int8_t);
    my $long_size  = 4;  # sizeof(int32_t);
    if ( $label->is_bound() ) {
        my $offs = $label->pos() - pc_offset() - 1;
        die if ( $offs > 0 );
        if ( is_int8( $offs - $short_size ) && !predictable_code_size() ) {
            # 1110 1011    #8-bit disp.
            emit(0xEB);
            emit( ( $offs - $short_size ) & 0xFF );
        }
        else {
            # 1110 1001    #32-bit disp.
            emit(0xE9);
            emitl( $offs - $long_size );
        }
    }
    else {
        die "jmp: don't know what to do with @_";
    }
}

sub _leave {
    emit(0xC9);
}

sub _movl {
    my ( $dst, $src ) = @_;
    if ( is_register($dst) && is_register($src) ) {
        if ($src->low_bits() == 4) {
            emit_optional_rex_32($src, $dst);
            emit(0x89);
            emit_modrm($src, $dst);
        }
        else {
            emit_optional_rex_32($dst, $src);
            emit(0x8B);
            emit_modrm($dst, $src);
        }
    }
    else {
        die "movl: don't know what to do with $dst, $src";
    }
}

sub _movq {
    my ( $dst, $src ) = @_;
    if ( is_register($dst) && is_register($src) ) {
        if ($src->low_bits() == 4) {
            emit_rex_64($src, $dst);
            emit(0x89);
            emit_modrm($src, $dst);
        }
        else {
            emit_rex_64($dst, $src);
            emit(0x8B);
            emit_modrm($dst, $src);
        }
    }
    else {
        die "movq: don't know what to do with $dst, $src";
    }
}

sub _movsxlq {
    my ( $dst, $src ) = @_;
    if ( is_register($dst) && is_register($src) ) {
        emit_rex_64($src, $dst);
        emit(0x63);
        emit_modrm($src, $dst);
    }
    else {
        die "movsxlq: don't know what to do with $dst, $src";
    }
}

sub _repmovsb() {
    emit(0xF3);
    emit(0xA4);
}

sub _repmovsw() {
    emit(0x66);    # Operand size override.
    emit(0xF3);
    emit(0xA4);
}

sub _repmovsl() {
    emit(0xF3);
    emit(0xA5);
}

sub _repmovsq() {
    emit(0xF3);
    emit_rex_64();
    emit(0xA5);
}

sub _mul {
    my ($src) = @_;
    emit_rex_64($src);
    emit(0xF7);
    emit_modrm( 0x4, $src );
}

sub _neg {
    my ($dst) = @_;
    emit_rex_64($dst);
    emit(0xF7);
    emit_modrm( 0x3, $dst );
}

sub _negl {
    my ($dst) = @_;
    emit_optional_rex_32($dst);
    emit(0xF7);
    emit_modrm( 0x3, $dst );
}

sub _nop {
    emit(0x90);
}

sub _pop {
    my ( $dst ) = @_;
    if ( is_register($dst) ) {
        $dst->emit_optional_rex_32();
        emit(0x58 | $dst->low_bits());
    }
    else {
        die "pop: don't know what to do with $dst";
    }
}

sub _popfq {
    emit(0x9D);
}

sub _push {
    my ( $src ) = @_;
    if ( is_register($src) ) {
        $src->emit_optional_rex_32();
        emit(0x50 | $src->low_bits());
    }
    elsif (is_int8($src)) {
        emit(0x6A);
        emit($src);     # Emit low byte of value.
    }
    elsif (!ref($src)) {
        emit(0x68);
        emitl($src);    # int32
    }
    else {
        die "push: don't know what to do with $src";
    }
}

# Push a 32 bit integer, and guarantee that it is actually pushed as a
# 32 bit value, the normal push will optimize the 8 bit case.
sub _push_imm32 {
    my ( $src ) = @_;
    emit(0x68);
    emitl($src);    # int32
}

sub _pushfq {
    emit(0x9C);
}

sub _rdtsc {
    emit(0x0F);
    emit(0x31);
}

sub _ret {
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

sub _shld {
    my ( $dst, $src ) = @_;
    emit_rex_64($src, $dst);
    emit(0x0F);
    emit(0xA5);
    emit_modrm($src, $dst);
}

sub _shrd {
    my ( $dst, $src ) = @_;
    emit_rex_64($src, $dst);
    emit(0x0F);
    emit(0xAD);
    emit_modrm($src, $dst);
}

sub _xchg {
    my ( $dst, $src ) = @_;
    if ( $src->is(rax) || $dst->is(rax) ) {
        # Single-byte encoding
        my $other = $src->is(rax) ? $dst : $src;
        emit_rex_64($other);
        emit( 0x90 | $other->low_bits() );
    }
    elsif ( $dst->low_bits() == 4 ) {
        emit_rex_64( $dst, $src );
        emit(0x87);
        emit_modrm( $dst, $src );
    }
    else {
        emit_rex_64( $src, $dst );
        emit(0x87);
        emit_modrm( $src, $dst );
    }
}

1;

__END__

=pod

=head1 Perlito5::X64::Assembler

The Perlito5 x64 backend

=head1 Synopsis

    use Perlito5::X64::Assembler;

    package Perlito5::X64::Assembler;
    _ret();
    say to_hex();   # C3

    asm_reset();
    my $here = label;
    _xchg( rax, rcx );
    _bind($here);
    say "# xchg " . to_hex();
    say "# label pos=", $here->pos();

=head1 References

The API follows approximately the V8 Javascript compiler assembler:

    src/x64/assembler-x64.cc

This is the copyright message from V8:

    Copyright 2012 the V8 project authors. All rights reserved.
    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:

        * Redistributions of source code must retain the above copyright
          notice, this list of conditions and the following disclaimer.
        * Redistributions in binary form must reproduce the above
          copyright notice, this list of conditions and the following
          disclaimer in the documentation and/or other materials provided
          with the distribution.
        * Neither the name of Google Inc. nor the names of its
          contributors may be used to endorse or promote products derived
          from this software without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=cut


