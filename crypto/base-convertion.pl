#!/usr/bin/perl -w
use strict;

my %b64 = (
    A => '000000',
    B => '000001',
    C => '000010',
    D => '000011',
    E => '000100',
    F => '000101',
    G => '000110',
    H => '000111',
    I => '001000',
    J => '001001',
    K => '001010',
    L => '001011',
    M => '001100',
    N => '001101',
    O => '001110',
    P => '001111',
    
    Q => '010000',
    R => '010001',
    S => '010010',
    T => '010011',
    U => '010100',
    V => '010101',
    W => '010110',
    X => '010111',
    Y => '011000',
    Z => '011001',
    a => '011010',
    b => '011011',
    c => '011100',
    d => '011101',
    e => '011110',
    f => '011111',
    
    g => '100000',
    h => '100001',
    i => '100010',
    j => '100011',
    k => '100100',
    l => '100101',
    m => '100110',
    n => '100111',
    o => '101000',
    p => '101001',
    q => '101010',
    r => '101011',
    s => '101100',
    t => '101101',
    u => '101110',
    v => '101111',
    
    w => '110000',
    x => '110001',
    y => '110010',
    z => '110011',
    0 => '110100',
    1 => '110101',
    2 => '110110',
    3 => '110111',
    4 => '111000',
    5 => '111001',
    6 => '111010',
    7 => '111011',
    8 => '111100',
    9 => '111101',
    '+' =>'111110',
    '/' =>'111111',
);

my %hex = (
    0 => '0000',
    1 => '0001',
    2 => '0010',
    3 => '0011',
    4 => '0100',
    5 => '0101',
    6 => '0110',
    7 => '0111',
    8 => '1000',
    9 => '1001',
    A => '1010',
    B => '1011',
    C => '1100',
    D => '1101',
    E => '1110',
    F => '1111',
);

# encoded string to bit string
sub enc_to_bits {
    my( $es, $bits_rep ) = @_;
    my $bits;

    while (length($es) > 0) {
	my $digit = substr($es, 0, 1); # get first digit
	$es = substr($es, 1);	# and move along
	$bits .= $$bits_rep{$digit};
    }

    return $bits;
}

# bits string to encoded format
sub bits_to_enc {
    my( $bits, $bits_rep ) = @_;
    my $es;
    my $len;			# bits per digit

    my @values = @$bits_rep{keys %$bits_rep};
    $len = length( $values[0] );

    while (length($bits) > 0) {
	my $b = substr($bits, 0, $len);
	$bits = substr($bits, $len);
	$es .= bits_to_digit($b, $bits_rep);
    }

    return $es;
}

# convert bits to digit
sub bits_to_digit {
    my ($bits, $bits_rep) = @_;

    for (keys %$bits_rep) {
	if ($$bits_rep{$_} eq $bits) {
	    return $_;
	}
    }
    die "Digit not found\n";
}

# convert digit to bits
sub digit_to_bits {
    my( $bits, $bits_rep ) = @_;

    for (keys %$bits_rep) {
	if ($$bits_rep{$_} eq $bits) {
	    return $_;
	}
    }
    die "Bit string error";
}
