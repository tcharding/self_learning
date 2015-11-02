#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use Data::Dumper;
use Crypto::Util;
use Crypto::Rate;

my $MIN_KLEN = 2;
my $MAX_KLEN = 40;
my $BIG = 255;			# used as initial value in < comparison

#my @keysizes = (3, 5, 8, 2, 24, 15, 9, 18, 36); # result from &guess_keysize

my $file = "6.txt";
open my $fh, '<', $file or die "Cannot open file: $file\n";

my $input;
while ( <$fh> ) {
    chomp;
    $input .= $_;
}
my $c = verify_and_decode( $input );
&friedman_method( $c );
#&guess_keysize2( $c );
#&guess_keysize4( $c );
#&guess_keysize_all( $c );

sub friedman_method {
    my $c = shift;
    print "guessing keysize using Friedman method: \n";

    for ($MIN_KLEN .. $MAX_KLEN) {	# from question
	my $key_nchars = $_;
	my $key_bits = $_ * 8;
	my $shifted = $c;
	my $nshifts;
	my $ic = 0;
	
	while( length( $shifted) >= $key_bits) {
	    $shifted = substr( $shifted, $key_bits );
	    $nshifts++;
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
	    $ic += ($same / $total);
	}
	$ic /= $nshifts;
#	if ($ic > 0.04) {
	    print "keylen: $key_nchars ($ic)\n";
#	}
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

# return list of 3 keys with smallest values
sub smallest_three {
    my $edit_distance = shift;

    my( $one, $two, $three ) = (1, 1, 1); # ugly hack: set initial values to big
    $$edit_distance{ 1 } = $BIG;
    				# loop hash storing smallest three key lengths
    for ($MIN_KLEN .. $MAX_KLEN) {	
	if ($$edit_distance{ $_ } < $$edit_distance{ $one }) {
	    $one = $_;
	} elsif ($$edit_distance{ $_ } < $$edit_distance{ $two }) {
	    $two = $_;
	} elsif ($$edit_distance{ $_ } < $$edit_distance{ $three }) {
	    $three = $_;
	}
    }
    print "($one, $two, $three)\n";
}

sub guess_keysize4 {
    print "guessing keysize using 4 blocks: ";
    my $c = shift;
    my %edit_distance;
    
    for my $keysize ($MIN_KLEN .. $MAX_KLEN) {	# from question
	my $nbits = $keysize * 8;
	
	my $first = substr( $c, 0, $nbits );
	my $second = substr( $c, $nbits, $nbits);
	my $third = substr( $c, $nbits*2, $nbits);
	my $fourth = substr( $c, $nbits*3, $nbits);

	my $hd;
	$hd += &hamming( $first, $second );
	$hd += &hamming( $first, $third );
	$hd += &hamming( $first, $fourth );
	$hd += &hamming( $second, $third );
	$hd += &hamming( $second, $fourth );
	$hd += &hamming( $third, $fourth );
	$hd /= 6;
	my $normalized = $hd / $keysize;
	$edit_distance{ $keysize } = $normalized;
    }
    return smallest_three( \%edit_distance );
}

sub guess_keysize2 {
    print "guessing keysize using 2 blocks: ";
    my $c = shift;
    my %edit_distance;
    
    for my $keysize ($MIN_KLEN .. $MAX_KLEN) {	# from question
	my $nbits = $keysize * 8;
	
	my $first = substr( $c, 0, $nbits );
	my $second = substr( $c, $nbits, $nbits);

	my $hd;
	$hd += &hamming( $first, $second );
	my $normalized = $hd / $keysize;
	$edit_distance{ $keysize } = $normalized;
    }
    return smallest_three( \%edit_distance );
}

sub guess_keysize_all {
    print "guessing keysize using all blocks: ";
    my $input = shift;
    my %edit_distance;
    
    for my $keysize ($MIN_KLEN .. $MAX_KLEN) {	# from question
	my $c = $input;
	my $nbits = $keysize * 8;
	my( $hd_tot, $cnt );

	while( length( $c ) > ($nbits * 2)) {
	    $cnt++;
	    my $first = substr( $c, 0, $nbits );
	    my $second = substr( $c, $nbits, $nbits);
	    $hd_tot += &hamming( $first, $second );
	    $c = substr( $c, ($nbits * 2) );
	}
	$hd_tot /= $cnt; 	# loop should always be entered at least once
	my $normalized = $hd_tot / $keysize;
	$edit_distance{ $keysize } = $normalized;
    }
#    print Dumper(\%edit_distance);
    return smallest_three( \%edit_distance );
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
