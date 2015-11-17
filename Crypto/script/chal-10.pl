#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

#
# Implement CBC mode
#

use MIME::Base64 qw(encode_base64 decode_base64);
use Crypt::Rijndael;
use Crypto::Block qw/:all/;
use Crypto::Cipher qw/:all/;

my $key = "YELLOW SUBMARINE";
my $iv = "THIS IS 16 BYTES";

my $plaintext = "this is the top secret message";
$plaintext = pad( $plaintext, 16 );
my $ciphertext = encrypt_aes_cbc( $plaintext, $key, $iv );
my $washed = decrypt_aes_cbc( $ciphertext, $key, $iv );

print "Set 2 Challenge 10: ";
if ( $plaintext eq $washed ) {
    print "Completed!\n";
} else {
    print "Failed\n";
    print "expected: $plaintext\n";
    print "We got:   $washed\n";
}
