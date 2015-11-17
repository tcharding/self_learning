#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

#
# Clone an MT19937 RNG from its output
#
my $n = 2;
my $X = 0x630630AB;

my( $u, $d ) = (11, 0xFFFFFFFF);
my( $s, $b ) = (16, 0x9D2C5680);
my( $t, $c ) = (15, 0xEFC60000);
my $a = 0x9908B0DF;
my $l = 18;
my $f = 1812433253;

#&test_sb_xor();
#&test_bit_conversion();

print "do_undo() ";
&do_undo();
print "run_inversion() ";
&run_inversion();

sub inv_left_n_and {
    my( $y, $shift, $mask ) = @_;
    my $nrecovered = 0;

    my $r = $y;
    $nrecovered = $shift;

    while ($nrecovered < 32) {

	$r = $y ^ ($r << $shift & $mask);
	$nrecovered += $shift;
    }

    return $r;
}

sub doit {
    my( $y ) = @_;

#    $y = $y ^ (($y >> $u));	 # step u
    $y = $y ^ (($y << $s) & $b); # step s
#    $y = $y ^ (($y << $t) & $c); # step t
#    $y = $y ^ ($y >> $l);	 # step l
    
    return int32($y);
}
sub undoit {
    my( $y ) = @_;

#    $y = right_shift_n( $y, $l );
    $y = inv_s( $y );
#    $y = inv_t( $y );
#    $y = right_shift_n( $y, $u );

    return int32($y);
}

sub extract_number {
    my( $y ) = @_;
    print "\ne: \n$y\n";

    $y = &exu( $y );  print "$y\n";
    $y = &exs( $y );  print "$y\n";
    $y = &ext( $y );  print "$y\n";
    $y = &exl( $y );  print "$y\n";    

    return $y;
}
sub invert_temper {
    my( $y ) = @_;
    print "\ni: \n$y\n";

    $y = inv_l( $y ); print "$y\n";
    $y = &inv_t( $y ); print "$y\n";
    $y = &inv_s( $y ); print "$y\n";
    $y = &inv_u( $y ); print "$y\n";
    
    return $y;
#    return int32($y);
}
sub exu {
    my $y = shift;
    return $y ^ (($y >> $u));
}

# not used
sub inv_u {
    my $y = shift;
    print "not implemented\n";
    return $y;
}
sub exs {
    my $y = shift;
    return $y ^ (($y << $s) & $b);
}
sub inv_s {
    my $y = shift;

    return inv_left_n_and( $y, $s, $b );
}

sub ext {
    my $y = shift;
    return $y ^ (($y << $t) & $c);
}
sub inv_t {
    my $y = shift;

#    my $part = $y << $t;
#    $y = $y ^ ($y << $t & $c);
    $y = inv_left_n_and( $y, $t, $c );
    return $y;
}
sub exl {
    my $y = shift;
    return $y ^ ($y >> $l);
}
sub inv_l {
    my $y = shift;
    print "does not work\n";
    my $part = $y << $l;
    $y = $y ^ ($part & $y);

    return $y;
}

sub do_undo {
    my $in = int(rand(2**16));
    my $out = &doit( $in );
    my $inverted = &undoit( $out );

    print "... ";
    if ($inverted == $in) {
	print "Success!\n";
    } else {
	print "Failed\n";
    }
}

sub run_inversion {
    my $in = int(rand(2**16));
    my $out = &extract_number( $in );
    my $inverted = &invert_temper( $out );

    print "... ";
    if ($inverted == $in) {
	print "Success!\n";
    } else {
	print "Failed\n";
#	print "expect: $in\n";
#	print "we got: $inverted\n";
    }
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


sub test_bit_conversion {
    my $val = 630;
    my $bits = dec2bits( $val );
    my $rinsed = bits2dec( $bits );
    if ($rinsed != $val) {
	print "bit_conversion failed\n";
    }
}
