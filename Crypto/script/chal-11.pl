#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use Crypt::Rijndael;
use Crypto::Block qw/:all/;

my $file = "7.out.txt";
open my $fh, '<', $file or die "Cannot open $file";
my $message = do { local( $/ ) ; <$fh> } ;
#my $message = "this is the string that will be encrypted, oh yes it will";

#for (1 .. 20) {
#    my $key = &random_16_chars;
#    print "$key\n";
#}

for (1 .. 10) {
    my $c = encryption_oracle( $message );
    my $guess = ecb_or_cbc( $c );
    print "We guessed $guess\n";
}

sub ecb_or_cbc {
    my $hex = unpack('H*', shift);
    if (&has_repeats( $hex )) {
	return 0;		# ecb mode
    }
    return 1;			# cbc mode
}

sub encryption_oracle {
    my $random = "randomtext";
    my $iv = "THIS IS 16 BYTES";
    my $key = &random_16_chars;
    my $type = int(rand 2);
    print "we used: $type (ecb=0 cbc=1)\n";
    
    my $n = 0;
    while( $n < 5) {
	$n = int(rand(11));
    }
    my $append = substr($random, 0, $n);
    my $p = $append . $message . $append;

    if ($type == 0) {		# ecb mode
	my $input = &pad( $p, 16 );
	my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_ECB() );
	return $cipher->encrypt( $input );
    } else {			# cbc mode
	return &encrypt_aes_cbc( $p, $key, $iv );
    }
}

sub random_16_chars {
    my $key;

    for (1 .. 16) {
	# 32 - 126
	my $n = 0;
	while( $n < 32) {
	    $n = int(rand(126));
	}
	$key .= chr($n);
    }
    return $key;
}

sub has_repeats {
    my $hex = shift;
    my( $i, $j );
    my $chunk = 32;		# 32 hex digits = 16 bytes
    for( $i = 0; $i < length( $hex ); $i += $chunk ) {
	my $byte = substr( $hex, $i, $chunk );
	for( $j = $i + $chunk; $j < length( $hex ); $j += $chunk ) {
	    my $cmp = substr( $hex, $j, $chunk );
	    if( $byte eq $cmp ) {
		return 1;	# true
	    }
	}
    }
    return 0;			# false
}



