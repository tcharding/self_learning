#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use Data::Dumper;
use Crypto::Util;
use Crypto::Rate;

my @keysizes = (3, 5, 8, 2, 24, 15, 9, 18, 36); # result from &guess_keysize

my $file = "dev-6.txt";
open my $fh, '<', $file or die "Cannot open file: $file\n";

my $input;
while ( <$fh> ) {
    chomp;
    $input .= $_;
}
my $c = verify_and_decode( $input );
&guess_keylen( $c );

sub guess_keylen {
    my $c = shift;

    for ( 2..40 ) {
	my $key_nchars = $_;
	my $key_bits = $_ * 8;
	my $shifted = $c;
	
	while( length( $shifted) >= $key_bits) {
	    $shifted = substr( $shifted, $key_bits );
	    my( $same, $total ) = (0, 0);
	    my $len = length( $shifted );
	    for(my $i = 0; $i < $len; $i+=8 ) {
		if (substr($c, $i, 8) eq substr($shifted, $i, 8)) {
		    $same++;
		}
		$total++;
	    }
	    if ($same == 0) {
		next;
	    }
	    my $ic = 0;
	    $ic = $same / $total;
#	    print "$ic\n";

	    if ($ic > 0.05) {
		print "keylen: $key_nchars\n";
	    }
	}
    }
}

sub verify_and_decode {
    my $input = shift;
    my $ilen = length( $input );
    if( ($ilen * 6) % 8 != 0 ) {
	warn "input is not byte aligned, will need padding"
    }
    decode_b64($input);
}
