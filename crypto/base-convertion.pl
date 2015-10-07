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

# convert hex string to bit string
sub hextobits {
    my $s = shift;
    my $bits;
    while (length($s) > 0) {
	my $digit = substr($s, 0, 1);
	$s = substr($s, 1);
	$bits .= &hex_dtob($digit)
    }
    return $bits;
}

# convert base 64 string to bit string
sub b64tobits {
    my $s = shift;
    my $bits;
    while (length($s) > 0) {
	my $digit = substr($s, 0, 1);
	$s = substr($s, 1);
	$bits .= &b64_dtob($digit)
    }
    return $bits;
}

# convert bit string to base 64 string
sub bitstob64 {
    my $bits = shift;
    my $s;
    my $len = 6;
    if (length($bits) %  $len != 0) {
	die "bitstob4: string not mod $len\n";
    }
    while (length($bits) > 0) {
	my $b = substr($bits, 0, $len);
	$bits = substr($bits, $len);
	$s .= b64_btod($b);
    }
    return $s;
}

# convert bit string to hex string
sub bitstohex {
    my $bits = shift;
    my $s;
    my $len = 4;
    if (length($bits) %  $len != 0) {
	die "bitstohex: string not mod $len\n";
    }
    while (length($bits) > 0) {
	my $b = substr($bits, 0, $len);
	$bits = substr($bits, $len);
	$s .= b64_btod($b);
    }
    return $s;
}

# hex digit to bit string (length 4)
sub hex_dtob {
    my $digit = shift;
    for (keys %hex) {
	if ($_ eq $digit) {
	    return $hex{$_};
	}
    }
}

# base 64 digit to bit string (length 6)
sub b64_dotb {
    my $digit = shift;
    for (keys %b64) {
	if ($_ eq $digit) {
	    return $b64{$_};
	}
    }
}

# convert length 6 bit string to base 64 digit
sub b64_btod {
    my $bits = shift;
    for (keys %b64) {
	if ($b64{$_} eq $bits) {
	    return $_;
	}
    }
}

# convert length 4 bit string to hex digit
sub hex_btod {
    my $bits = shift;	     
    for (keys %hex) {
	if ($hex{$_} eq $bits) {
	    return $_;
	}
    }
}
