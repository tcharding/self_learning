#!/usr/bin/perl
use strict;
use warnings;

use Crypto::Util;

&test_hamming();
exit;

sub test_hamming {
    my $hd = &hamming( "this is a test", "wokka wokka!!!");

    if ( $hd != 37 ) {
	print "hamming() failed\n";
    }
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
