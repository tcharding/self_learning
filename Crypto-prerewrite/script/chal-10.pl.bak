#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use MIME::Base64 qw(encode_base64 decode_base64);
use Crypt::Rijndael;
use Crypto::Block qw/:all/;
use Crypto::Convert qw/:all/;

my $key = "YELLOW SUBMARINE";
my $iv = "THIS IS 16 BYTES";

my $plaintext = "this is the top secret message";
my $ciphertext = encrypt_aes_cbc( $plaintext, $key, $iv );
$plaintext = decrypt_aes_cbc( $ciphertext, $key, $iv );
print "$plaintext\n";

# AES 128 CBC mode, key is 16 byte string, iv is 32 digit hex string
sub encrypt_aes_cbc {
    my( $plaintext, $key, $iv ) = @_;
				# sanity checks
    if( length($iv) != 16) {	# 16 bytes
	die "IV must be 16 bytes long";
    }
    if( length($key) != 16) {	# 16 bytes
	die "key must be 16 bytes long";
    }
    				# set up cipher and $iv
    my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_ECB() );
    $iv = ascii_to_hex( $iv );

				# split message into blocks and hexify
    my $hex = ascii_to_hex( $plaintext );
    my $pblocks = split_into_blocks( $plaintext, 32 );	# 32 hex digits for 16 bytes

    				# now chain block cipher mode
    my( $bin, $ciphertetx );
    my $feedback = $iv;
    my $first = 0;
    for my $current (@$pblocks) {
	my $input = unpack('H*',pack('H*', $feedback) ^ pack('H*', $current));
	my $bin = $cipher->encrypt( $input );
	if ($first == 0) {
	    $ciphertext = $bin;
	    $first = 1;
	} else {
	    $ciphertext = pack('H*', unpack('H*', $ciphertext) . unpack('H*', $bin) );
	}
	$feedback = unpack('H*', $bin);
    }
    return $ciphertext;
}
# AES 128 CBC mode, all inputs are in ASCII
sub decrypt_aes_cbc {
    my( $ciphertext, $key, $iv ) = @_;
    				# sanity checks
    if( length($iv) != 16) {	# 16 bytes
	die "IV must be 16 bytes long";
    }
    if( length($key) != 16) {	# 16 bytes
	die "key must be 16 bytes long";
    }
    				# set up cipher and $iv
    my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_ECB() );
    $iv = ascii_to_hex( $iv );

				# split message into blocks and hexify
    $plaintext = ascii_to_hex( $plaintext );
    my $pblocks = split_into_blocks( $plaintext, 32 );	# 32 hex digits for 16 bytes

				# now chain block cipher mode
    my $feedback = $iv;
    my $hex;
    for (@$pblocks) {
	my $input = pack('H*', $_);
	my $hblock = $cipher->decrypt( $input );
	$hex .= unpack('H*',pack('H*', $feedback) ^ pack('H*', $hblock));
	$feedback = $_;
    }
    return hex_to_ascii( $hex );
}
