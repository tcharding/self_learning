#!/usr/bin/perl -w
use strict;

use lib qw(/home/tobin/build/github/self_learning/Crypto/lib);
use Crypto::Util qw( :all );

# upper case all hex strings
my $input = "1c0111001f010100061a024b53535009181c";
my $key   = "686974207468652062756c6c277320657965";
my $expected = uc "746865206b696420646f6e277420706c6179";

my $in_bits = decode_hex( $input );
my $key_bits = decode_hex( $key );

my $cipher = &repeating_xor($in_bits, $key_bits);
my $out = encode_hex( $cipher );

print "Set 1 Challenge 2: ";
if ($out eq $expected) {
    print "Completed!\n";
} else {
    print "Failed\n";
}

