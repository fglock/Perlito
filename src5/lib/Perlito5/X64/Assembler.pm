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


package Perlito5::X64::Immediate;

sub new {
    my $class = shift;
    bless {@_}, $class;
}


package Perlito5::X64::Operand;

sub new {
    # Register    base,
    # Register    index,
    # ScaleFactor scale,
    # int32_t     disp

    # byte rex_;
    # byte buf_[6];
    # byte len_   #  The number of bytes of buf_ in use.

    my $class = shift;
    bless {@_}, $class;
}

sub rsp () { Perlito5::X64::Assembler::rsp } 
sub r12 () { Perlito5::X64::Assembler::r12 } 

sub set_modrm { 
    my ($op, $mod, $rm_reg) = @_;
    die unless (is_uint2($mod)); 
    $op->{buf_}[0] = $mod << 6 | $rm_reg->low_bits(); 
    # Set REX.B to the high bit of rm.code(). 
    $op->{rex_} |= $rm_reg->high_bit(); 
} 
 
 
sub set_sib { 
    my ($op, $scale, $index, $base) = @_;
    die unless ($op->{len_} == 1); 
    die unless (is_uint2($scale)); 
    # Use SIB with no index register only for base rsp or r12. Otherwise we 
    # would skip the SIB byte entirely. 
    die unless (!$index->is(rsp) || $base->is(rsp) || $base->is(r12)); 
    $op->{buf_}[1] = ($scale << 6) | ($index->low_bits() << 3) | $base->low_bits(); 
    $op->{rex_} |= $index->high_bit() << 1 | $base->high_bit(); 
    $op->{len_} = 2; 
} 
 
sub set_disp8 { 
    my ($op, $disp) = @_;
    die unless (is_int8($disp)); 
    die unless ($op->{len_} == 1 || $op->{len_} == 2); 
    $op->{buf_}[$op->{len_}] = $disp;
    $op->{len_}++;
} 
 
sub set_disp32 { 
    my ($op, $disp) = @_;
    die unless ($op->{len_} == 1 || $op->{len_} == 2); 
    $op->{buf_}[$op->{len_} + 0] = ( $disp & 0xFF );
    $op->{buf_}[$op->{len_} + 1] = ( ( $disp >> 8  ) & 0xFF );
    $op->{buf_}[$op->{len_} + 2] = ( ( $disp >> 16 ) & 0xFF );
    $op->{buf_}[$op->{len_} + 3] = ( ( $disp >> 24 ) & 0xFF );
    $op->{len_} += 4;
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
    if ( ref($_[0]) eq 'Perlito5::X64::Immediate' ) {
        emitl($_[0]->{value});
    }
    else {
        push @buffer, $_[0];
    }
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

# emit REX if needed
sub emit_optional_rex_32 {
    my ($reg) = @_;
    if ( @_ == 1 && is_register($reg) ) {
        Perlito5::X64::Assembler::emit(0x41) if $reg->high_bit();
    }
    else {
        die "emit_optional_rex_32: don't know what to do with @_";
    }
}

sub emit_rex_64 {
    my ($reg, $rm_reg) = @_;
    if ( @_ == 0 ) {
        # Emit a REX prefix that only sets REX.W to choose a 64-bit operand size.
        emit(0x48);
    }
    elsif ( @_ == 1 && is_register($reg) ) {
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
        die "emit_rex_64: don't know what to do with @_";
    }
}

sub emit_operand {
    my ($reg, $op) = @_;
    if ( is_register($reg) && is_operand($op) ) {
        # Emit the ModR/M byte, and optionally the SIB byte and
        # 1- or 4-byte offset for a memory operand.  Also encodes
        # the second operand of the operation, a register or operation
        # subcode, into the reg field of the ModR/M byte.
        emit_operand($reg->low_bits(), $op);
    }
    elsif ( !ref($reg) && is_operand($op) ) {
        # Emit the ModR/M byte, and optionally the SIB byte and
        # 1- or 4-byte offset for a memory operand.  Also used to encode
        # a three-bit opcode extension into the ModR/M byte.

        # ASSERT(is_uint3(code));
        # const unsigned length = adr.len_;
        # ASSERT(length > 0);
        #
        # # Emit updated ModR/M byte containing the given register.
        # ASSERT((adr.buf_[0] & 0x38) == 0);
        # pc_[0] = adr.buf_[0] | code << 3;
        #
        # # Emit the rest of the encoded operand.
        # for (unsigned i = 1; i < length; i++) pc_[i] = adr.buf_[i];
        # pc_ += length;

        die "TODO";
    }
    else {
        die "emit_operand: don't know what to do with @_";
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
        die "emit_modrm: don't know what to do with @_";
    }
}

sub is_label {
    ref($_[0]) eq 'Perlito5::X64::Label'
}

sub is_register {
    ref($_[0]) eq 'Perlito5::X64::Register'
}

sub is_immediate {
    ref($_[0]) eq 'Perlito5::X64::Immediate'
}

sub is_operand {
    ref($_[0]) eq 'Perlito5::X64::Operand'
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

sub Immediate {
    return Perlito5::X64::Immediate->new( value => $_[0] );
}

sub Operand {
    # Register    base,
    # Register    index,
    # ScaleFactor scale,
    # int32_t     disp

    # byte rex_;
    # byte buf_[6];
    # byte len_   #  The number of bytes of buf_ in use.

    if ( @_ == 2 ) {
        #  [base + disp/r]
        # Operand(Register base, int32_t disp);

        my $op = Perlito5::X64::Operand->( base => $_[0], disp => $_[1] );

        $op->{rex_} = 0;
        $op->{len_} = 1;
        if ($op->{base}->is(rsp) || $op->{base}->is(r12)) {
            # SIB byte is needed to encode (rsp + offset) or (r12 + offset).
            $op->set_sib(times_1, rsp, $op->{base});
        }

        if ($op->{disp} == 0 && !$op->{base}->is(rbp) && !$op->{base}->is(r13)) {
            $op->set_modrm(0, $op->{base});
        }
        elsif (is_int8($op->{disp})) {
            $op->set_modrm(1, $op->{base});
            $op->set_disp8($op->{disp});
        } 
        else {
            $op->set_modrm(2, $op->{base});
            $op->set_disp32($op->{disp});
        }

        return $op;
    }
    elsif ( @_ == 3 ) {
        #  [index*scale + disp/r]
        # Operand(Register index,
        #         ScaleFactor scale,
        #         int32_t disp);

        die "TODO";
        return Perlito5::X64::Operand->( index => $_[0], scale => $_[1], disp => $_[2] );
    }
    elsif ( @_ == 4 ) {
        #  [base + index*scale + disp/r]
        # Operand(Register base,
        #         Register index,
        #         ScaleFactor scale,
        #         int32_t disp);

        die "TODO";
        return Perlito5::X64::Operand->( base => $_[0], index => $_[1], scale => $_[2], disp => $_[3] );
    }
    else {
        # TODO:
        #  Offset from existing memory operand.
        #  Offset is added to existing displacement as 32-bit signed values and
        #  this must not overflow.
        # Operand(const Operand& base, int32_t offset);

        die "TODO";
        die "Operand: don't know what to do with @_";
    }
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
    if ( is_label($label) && $label->is_bound() ) {
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
    elsif ( is_register($label) ) {
        my $target = $label; 
        # Opcode FF/4 r64.
        emit_optional_rex_32($target);
        emit(0xFF);
        emit_modrm(0x4, $target);
    }
    else {
        die "jmp: don't know what to do with @_";
    }
}

sub _leave {
    emit(0xC9);
}

sub _movb {
    my ( $dst, $src ) = @_;
    if ( @_ == 2 && is_register($dst) && is_immediate($src) ) {
        # Immediate value
        if (!$dst->is_byte_register()) {
          emit_rex_32($dst);
        }
        emit(0xB0 + $dst->low_bits());
        emit($src->{value});
    }
    else {
        die "movb: don't know what to do with $dst, $src";
    }
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
    elsif ( @_ == 2 && is_register($dst) && is_immediate($src) ) {
        # Immediate value
        my $value = $src;
        emit_optional_rex_32($dst);
        emit(0xB8 + $dst->low_bits());
        emit($value);
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
    elsif ( @_ == 2 && is_register($dst) && is_immediate($src) ) {
        # Immediate value
        my $value = $src;
        emit_rex_64($dst);
        emit(0xC7);
        emit_modrm(0x0, $dst);
        emit($src);  # Only 32-bit immediates are possible, not 8-bit immediates.
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
        emit_optional_rex_32($dst);
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
        emit_optional_rex_32($src);
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

sub _syscall {
    emit(0x0F);
    emit(0x05);
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


=head1 NAME

Perlito5::X64::Assembler - x64 code generator for Perlito


=head1 SYNOPSIS

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


=head1 DESCRIPTION

This module generates "x64 native machine code" for the Perlito compiler.


=head1 REFERENCES

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


=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.


=head1 COPYRIGHT

Copyright 2013 by Flavio Soibelmann Glock.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>


=cut

