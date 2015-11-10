#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;
use Data::Dumper;
#
# Single-byte XOR cipher
#
# Perform Cryptanalisis
#

use Crypto::Base qw( hex_to_ascii );
use Crypto::Vigenere qw(:all);

my $c = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736";
say "Set 1 Challenge 3, performing cryptanalysis on ciphertext ...";

my $scx = &bruteforce_scx( $c );
&rate_msgs( $scx );
my $hex = &get_top_rated( $scx );	# returns top rated or undefined

if ( defined $hex ) {
    printf "%s\n", &hex_to_ascii( $hex );
} else {
    print "Unable to determine top rated message\n";
}


