#!/usr/bin/perl -w
use strict;
use warnings;

use lib qw(/home/tobin/build/github/self_learning/Crypto/lib);
use Crypto::Util qw( decode_hex encode_b64 );

my $input = uc "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d";
#$input = uc $input;
my $expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t";

my $bits = decode_hex( $input );
my $out = encode_b64( $bits );

print "Set 1 Challenge 1: ";
if ($out eq $expected) {
    print "Completed!\n";
} else {
    print "Failed\n";
}

