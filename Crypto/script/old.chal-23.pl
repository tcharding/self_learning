#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

#
# Clone an MT19937 RNG from its output
#
my $n = 2;
my $X = 0x630630AB;

&test_sb_xor();
&test_bit_conversion();

my $in = int(rand(2**16));
my $out = &doit( $in );
my $inverted = undoit( $out );

print "Invertion: ";
if ($inverted == $in) {
    print "Success!\n";
} else {
    print "Failed\n";
}

sub doit {
    my $a = shift;
    $b = $a ^ (($a << $n) & $X);

    return int32($b);
}

sub undoit {
    my $b = shift;
    my $a = left_shift_n_and( $b );
}


# inverts y = y ^ ((  y << N) & X)
sub left_shift_n_and {
    my $b = shift;
    my $input = dec2bits($b);
    my $shifted = "0" x $n;		# most significant bit (leftmost)
    my $output = "";
				# build $output
    for (1 .. 32) {
	my $bit = &sb_xor(substr($input, (0-$_), 1), substr($shifted, (0-$_), 1));
	$output = $bit . $output;
	$shifted = $bit . $shifted;
    }
    return bits2dec($output);
}

# inverts y = y ^ (y << N)
sub left_shift_n {
    my $b = shift;
    my $input = dec2bits($b);
    my $shifted = "0" x $n;		# most significant bit (leftmost)
    my $output = "";
				# build $output
    for (1 .. 32) {
	my $bit = &sb_xor(substr($input, (0-$_), 1), substr($shifted, (0-$_), 1));
	$output = $bit . $output;
	$shifted = $bit . $shifted;
    }
    return bits2dec($output);
}

# inverts y = y ^ (y >> N)
sub right_shift_n {
    my $b = shift;
    my $input = dec2bits($b);
    my $shifted = "0" x $n;		# most significant bit (leftmost)
    my $output = "";
				# build $output
    for (0 .. 31) {
	my $bit = &sb_xor(substr($input, $_, 1), substr($shifted, $_, 1));
	substr($output, $_, 1) = $bit;
	if ($_ < (32 - $n)) {		# exclude last loop iteration
	    substr($shifted, ($n + $_), 1) = $bit;   
	}
    }
    return bits2dec($output);
}

# inverts y = y ^ (y >> 1)
sub rigth_shift_1 {
    my $b = shift;
    my $input = dec2bits($b);
    my $shifted = "0";		# most significant bit (leftmost)
    my $output = "";
				# build $output
    for (0 .. 31) {
	my $bit = &sb_xor(substr($input, $_, 1), substr($shifted, $_, 1));
	substr($output, $_, 1) = $bit;
	if ($_ < (32 - 1)) {		# exclude last loop iteration
	    substr($shifted, (1 + $_), 1) = $bit;   
	}
    }
    return bits2dec($output);
}

# string (single) bit xor
sub sb_xor {
    my( $a, $b ) = @_;
    my $sum = $a + $b;
    if ($sum == 1) {
	return "1";
    } else {
	return "0";
    }
}

sub bits2dec {
    my $bits = shift;
    $bits = "0b" . $bits;
    oct($bits);
}

sub dec2bits {
    my $str = unpack("B32", pack("N", shift));    
}


# simulate MT19937 tempering
sub temper {
    no warnings;
    my $y = shift;

my( $u, $d ) = (11, 0xFFFFFFFF16);
my( $s, $b ) = (7, 0x9D2C568016);
my( $t, $c ) = (15, 0xEFC6000016);
    
    $y = $y ^ (($y >> $u) & $d);
    $y = $y ^ (($y << $s) & $b);
    $y = $y ^ (($y << $t) & $c);
    $y = $y ^ ($y >> 1);

    return int32( $y );
}

# Get the 32 least significant bits.
sub int32 {
    my $n = shift;
    return int(0xFFFFFFFF & $n)
}
sub test_sb_xor {
    if (sb_xor("0", "0") ne "0") {
	die "fail 0 0\n";
    }
    if (sb_xor("1", "1") ne "0") {
	die "fail 1 1\n";
    }
    if (sb_xor("0", "1") ne "1") {
	die "fail 0 1\n";
    }
    if (sb_xor("1", "0") ne "1") {
	die "fail 0 0\n";
    }
    
}

sub test_bit_conversion {
    my $val = 630;
    my $bits = dec2bits( $val );
    my $rinsed = bits2dec( $bits );
    if ($rinsed != $val) {
	print "bit_conversion failed\n";
    }
}
