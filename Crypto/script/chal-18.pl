#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

#
# Implement CTR, the stream cipher mode
#

use MIME::Base64 qw(encode_base64 decode_base64);
use Crypt::Rijndael;
use Crypto::Block qw/:all/;
use Crypto::Base qw( hex_to_ascii );

my $key = "YELLOW SUBMARINE";
my $c = "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ==";
$c = decode_base64( $c );
my $nonce = 0;
my $counter = 0;

my $p = decrypt_aes_ctr( $c, $nonce, $counter );
print "$p\n";

sub test_ctr_mode {
#my $p = decrypt_aes_ctr( $c, $nonce, $counter );
    my $p = "this is the message";
    my $c = encrypt_aes_ctr( $p, $nonce, $counter );
    my $rinsed = decrypt_aes_ctr( $c, $nonce, $counter );

    print "ctr mode";
    if ($p ne $rinsed) {
	print " broken\n";
	print "p: $p\n";
	print "r: $rinsed\n";
    } else {
	print " implemented\n";
    }    
}

# uses 64 bit nonce and 64 bit counter 
sub encrypt_aes_ctr {
    my( $p, $nonce, $counter ) = @_;
    my $ks = "";		# 16 bytes of key stream
    my $c;

    while (length($p) > 0) {
	if (length( $ks ) == 0) {
	    $ks = get_stream( $nonce, $counter );
	    $counter++;
	}

	$c .= pack('H*', substr($ks, 0, 2)) ^ pack('A*', substr($p, 0, 1));

				# move along
	$ks = substr($ks, 2);	# hex
	$p = substr($p, 1);	# ascii
    }
    return $c;
}

# uses 64 bit nonce and 64 bit counter
sub decrypt_aes_ctr {
    my( $c, $nonce, $counter ) = @_;
    my $ks = "";
    my $hex = unpack('H*', $c);
    my $p = "";
    
    while (length($hex) > 0) {
	if (length( $ks ) == 0) {
	    $ks = get_stream( $nonce, $counter );
	    $counter++;
	}

	my $hchar = substr($p, 0, 2);	      # hex
	my $kchar = substr($ks, 0, 2); # hex
	my $xor = pack('H*', $kchar) ^ pack('H*', $hchar);
	$p .= pack('H*', substr($ks, 0, 2)) ^ pack('H*', substr($hex, 0, 2));

				# move along
	$ks = substr($ks, 2);	# hex
	$hex = substr($hex , 2); # hex
    }
    return hex_to_ascii(unpack('H*', $p));
}

# used by ctr mode to get keystream
sub get_stream {
    my( $nonce, $counter ) = @_;
    my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_ECB() );
    
    my $hex = dec_to_64bit_hex( $nonce ) . dec_to_64bit_hex( $counter );
    my $c = $cipher->encrypt( hex_to_ascii( $hex ) );

    my $stream = unpack('H*', $c);
    return $stream;
}

# convert integer to 64 bit unsigned little endian hex string
sub dec_to_64bit_hex {
    my $dec = shift;
    my $hex = unpack('H*', pack('C*', $dec));
    while (length( $hex ) < 16 ) {
	$hex .= "00";
    }
    return $hex;
}

sub dump_hex {
    my $hex = shift;
    while (length($hex) > 0) {
	my $h = substr($hex, 0, 2);
	$hex = substr($hex, 2);
	print "0x$h ";
    }
    print "\n";
}
