#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

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
my $plaintext = hex_to_ascii( &sift( $scx ) );

if (defined $plaintext) {
    print "$plaintext\n";
} else {
    say "Cannot determine top rated plain text";
}

sub sift {
    my $scx = shift;
    &rate_msgs( $scx );
    get_top_rated( $scx );	# returns top rated or undefined
}
