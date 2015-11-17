#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

#
# From chal-23.pl: Implementation using bit strings, keep for reference
#

# inverts y = y ^ (y >> N)
sub inv_right_n {
    my( $b, $n ) = @_;
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

sub test_bit_conversion {
    my $val = 630;
    my $bits = dec2bits( $val );
    my $rinsed = bits2dec( $bits );
    if ($rinsed != $val) {
	print "bit_conversion failed\n";
    }
}
