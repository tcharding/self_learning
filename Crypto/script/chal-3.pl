#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use lib qw(/home/tobin/build/github/self_learning/Crypto/lib);
use Crypto::Convert qw(:all);
use Crypto::Analysis qw(:all);
use Data::Dumper;

my $hex = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736";
say "Set 1 Challenge 3, performing cryptanalysis on ciphertext ...";

#my $bin = pack( 'H*', $hex );
my $scx = &bruteforce_scx( $hex );
my $plaintext = &sift( $scx );

if (defined $plaintext) {
    print "$plaintext\n";
} else {
    say "Cannot determine top rated plain text";
}

sub sift {
    my $scx = shift;
    &rate_msgs( $scx );
#    print Dumper($scx);
    my $hex = get_top_rated( $scx );
    return hex_to_ascii( $hex );
}




