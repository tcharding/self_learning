#!/usr/bin/perl
use strict;
use warnings;


# xor input bit string against key
sub repeating_xor {
    my( $in, $key ) = @_;
    my $out;
    my $klen = length( $key );

    for ( my $i = 0; $i < length( $in ); $i++ ) {
	my $ki = $i % $klen;
	$out .= &xor_bit( substr( $in, $i, 1 ), substr( $key, $ki, 1 ));
    }
    return $out;
}

# xor to single bit strings i.e "1" or "0"
sub xor_bit {
    my( $a, $b ) = @_;
    
    if ( (($a eq "1") && ($b eq "1")) ||
	     (($a eq "0") && ($b eq "0")) ) {
	return "0";
    } elsif ( (($a eq "1") && ($b eq "0")) ||
		(($a eq "0") && ($b eq "1")) ) {
	return "1";
    } else {
	die "xor: unknown value (a: $a b: $b)\n";
    }
}
