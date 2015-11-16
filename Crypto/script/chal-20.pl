#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;
use autodie;

#
# Break fixed-nonce CTR mode using substitions
#

use MIME::Base64 qw(encode_base64 decode_base64);
use Crypto::Base qw( :all );
use Crypto::Stream qw( :all );
use Crypto::Vigenere qw( transpose rate_msgs get_top_rated );

my $key = &pseudo_random_string;
my $nonce = 0;
my @ciphertexts;

my $file = "script/18.txt";
open my $fh, '<', $file;
while (<$fh>) {
    chomp;
    my $counter = 0;
    my $p = decode_base64( $_ );
    my $c = encrypt_aes_ctr( $p, $key, $nonce, $counter );
    push @ciphertexts, unpack('H*', $c);
}

my $num = @ciphertexts;
my $max = -1;
for (@ciphertexts) {
    my $len = length( $_ );
    if ( $len > $max ) {
	$max = $len;
    }
}
my $allc;
for (@ciphertexts) {
    $allc .= substr( $_, 0, $max );
}

#
# break $allc as if it were repeating xor with keylen $max
#
print "max: $max\n";
my $keystring = build_key( $allc, $max );
print "$keystring\n";


# single char xor each block to find key character
sub build_key {
    my( $hex, $keylen ) = @_;
    my $trans_blocks = &transpose( $hex , $keylen);
    my( $key, $fail );

    $fail = 0;
    for (@$trans_blocks) {
	my $scx = &bruteforce_scx( $_ );
	&rate_msgs( $scx );
	my $top_hex = get_top_rated( $scx );
	if (defined $top_hex) {
	    for my $hb (keys %$scx) {
		my $this_hex = $$scx{ $hb }{ hex };
		if ( $this_hex eq $top_hex ) {
		    $key .= $hb;
		}
	    }
	} else {
	    $key .= ".";
	    $fail = 1;
	}
    }
    return $key;
}

sub bruteforce_scx {
    my $hex = shift;
    my %scx;

    my $len = length( $hex );
    for ( 0 .. 255 ) {
	my $char = chr($_);
	my $xor;
	
	for (my $i = 0; $i < $len; $i+=2) {
	    my $byte = substr($hex, $i, 2);
	    $xor .= pack('H*', $byte) ^ pack('C*', $_);
	}
	my $key = sprintf qq|%X|, int( $_ );
	if (length( $key ) == 1) {
	    $key = "0" . $key;
	}
	$scx{ $key }{ hex } = unpack('H*', $xor);
    }
    return \%scx;
}

