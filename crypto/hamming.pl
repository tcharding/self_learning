#!/usr/bin/perl
use strict;
use warnings;

my $hd = &hamming( "this is a test", "wokka wokka!!!");

if ( $hd != 37 ) {
    print "hamming failed\n";
}

# compute edit distance of two same length ascii strings
sub hamming {
    my( $s, $t ) = @_;
    my $bs = decode_ascii( $s );
    my $bt = decode_ascii( $t );
    my $len = length( $bs );
    my $hd = 0;
    
    if ($len != length( $bt )) {
	warn "bug alert: strings are not same length\n";
    }

    for (my $i = 0; $i < $len; $i++) {
	if (substr( $bs, $i, 1 ) != substr( $bt, $i, 1 )) {
	    $hd++;
	}
    }
    return $hd;
}


# convert decimal integer to binary string
sub dec2bin {
    my $str = unpack("B32", pack("N", shift));
    $str =~ s/^0+(?=\d)//;	# otherwise you'll get leading zeros
    return $str;
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

# convert ascii string to binary
sub decode_ascii {
    my $s = shift;
    my $bits;
    for (split //, $s) {
	$bits .= byte_val( $_ );
    }
    return $bits;
}
