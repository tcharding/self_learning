package Crypto::Util;
use strict;
use Exporter;

our @ISA = ('Exporter');
our @EXPORT = qw(
		encode_hex encode_b64 encode_ascii
		decode_hex decode_b64 decode_ascii
		dec2bin repeating_xor byte_val
	);

my %b64_digit = (
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

my %hex_digit = (
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

# xor input bit string against key
sub repeating_xor {
    my( $in, $key ) = @_;
    my $out;
    my $klen = length( $key );

    for ( my $i = 0; $i < length( $in ); $i++ ) {
	my $ki = $i % $klen;
	$out .= &_xor_bit( substr( $in, $i, 1 ), substr( $key, $ki, 1 ));
    }
    return $out;
}
# xor to single bit strings i.e "1" or "0"
sub _xor_bit {
    my( $a, $b ) = @_;
    
    if ( (($a eq "1") && ($b eq "1")) ||
	     (($a eq "0") && ($b eq "0")) ) {
	return "0";
    } elsif ( (($a eq "1") && ($b eq "0")) ||
		(($a eq "0") && ($b eq "1")) ) {
	return "1";
    } else {
	die "xor_bit: unknown value (a: $a b: $b)\n";
    }
}

# convert binary string to base 16 string
sub encode_hex {
    my $bits = shift;
    _encode_int( $bits, \%hex_digit );
}
# convert binary string to base 64 string
sub encode_b64 {
    my $bits = shift;
    _encode_int( $bits, \%b64_digit );
}

# convert base 16 string to binary
sub decode_hex {
    my $hex = shift;
    _decode_int( $hex, \%hex_digit );
}
# convert base 64 string to binary
sub decode_b64 {
    my $hex = shift;
    _decode_int( $hex, \%b64_digit );
}

# convert ascii string to binary
sub decode_ascii {
    my $s = shift;
    my $bits;
    for (split //, $s) {
	$bits .= byte_val( $_ );
    }
    return $bits;
}

# convert binary string to ASCII string
sub encode_ascii {
    my $bits = shift;
    my $hex = _encode_int( $bits, \%hex_digit );
    unless ($hex) {
	die "encode_ascii: no hex value\n  bits: $bits\n";
    }
    $hex =~ s/(([0-9a-f][0-9a-f])+)/pack('H*', $1)/ie;
    return $hex;
}

# binary string to encoded format TODO: add padding
sub _encode_int {
    my( $bits, $digits ) = @_;
    my ( $es, $bpd );
				# get bits per digit
    my @values = @$digits{keys %$digits};
    $bpd = length( $values[0] );
				# convert
    while( length( $bits ) > 0 ) {
	($bits, $b) = _first_nchars( $bits, $bpd );
	$es .= _bits_to_digit($b, $digits);
    }
    return $es;
}

# convert encoded string to binary format
sub _decode_int {
    my( $es, $digits ) = @_;
    $es = uc $es;		# all our digits are uppercase
    my( $bits, $digit );

    while( length( $es ) > 0 ) {
	($es, $digit) = _first_nchars($es, 1);
	$bits .= $$digits{ $digit };
    }
    return $bits;
}

#convert bits to digit
sub _bits_to_digit {
    my ($bits, $digits) = @_;
    for (keys %$digits) {
	if ($$digits{$_} eq $bits) {
	    return $_;
	}
    }
    die "Digit not found: '$bits'\n";
}

# return byte string of character ASCII value
sub byte_val {
    my $char = shift;
    my $decimal = ord( $char );
    my $byte = dec2bin( $decimal );
    # pad it to 8 bits
    while ( length($byte) < 8) {
	$byte = "0" . $byte;
    }
    return $byte;
}

# convert decimal integer to binary string
sub dec2bin {
    my $str = unpack("B32", pack("N", shift));
    $str =~ s/^0+(?=\d)//;	# otherwise you'll get leading zeros
    return $str;
}
# return first nchars of s and rest of s
sub _first_nchars {
    my( $s, $nchars ) = @_;
    my $chars = substr( $s, 0, $nchars );
    $s = substr( $s, $nchars );
    return( $s, $chars );
}

1;
