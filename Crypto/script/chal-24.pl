#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

#
# Create the MT19937 stream cipher and break it
#

use Crypto::Base qw( hex_to_ascii );
use Crypto::Prng;
use Digest::SHA qw(sha512);

my $seed = int(rand(2**16));

&test_enc_dec;
				# from question
my $p = pseudo_random_string( int(rand(9)) + 8 ); # 8 - 16 bytes;
$p .= "A" x 14;
my $c = encrypt_prng( $p );

my $key = &attack_cipher( $c );

# attack prng stream cipher
sub attack_cipher {
    my $c = shift;
}

# print "Set 3 Challenge 24: ";
# if ($p eq $rinsed) {
#     print "Completed\n";
# } else {
#     print "Failed\n";
#     print "expect: $p\n";
#     print "we got: $rinsed\n";
# }

sub stream_byte {
    my $prng = shift;
    my $v = $prng->extract_number;
    my $digest = sha512( $v );
    return $digest & pack('H*', "FF");
}

# encrypt using prng as a key stream
sub encrypt_prng {
    my $p = shift;
    my $len = length($p);
    my $c;
				# init prng
    my $prng = Crypto::Prng->new();
    $prng->seed( $seed );
    
    for (my $i = 0; $i < $len; $i++) {
	$c .= pack('A*', substr($p, $i, 1)) ^ &stream_byte($prng);
    }
    return $c;
}

# decrypt using prng as a key stream
sub decrypt_prng {
    my $c = shift;
    my $hex = unpack('H*', $c);
    my $len = length($hex);
    my $p;
				# init prng
    my $prng = Crypto::Prng->new();
    $prng->seed( $seed );
    
    for (my $i = 0; $i < $len; $i+=2) {
	$p .= pack('H*', substr($hex, $i, 2)) ^ &stream_byte($prng);
    }
    return hex_to_ascii( $p );
}
sub test_enc_dec {
    my $p = "this is the message";
    my $c = &encrypt_prng( $p );
    my $rinsed = decrypt_prng( $c );

    if ($p ne $rinsed) {
	print "encrypt/decrypt error:\n";
	print "expect: $p\n";
	print "we got: $rinsed\n\n";
    }
}
