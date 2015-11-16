#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

#
# Implement the MT19937 Mersenne Twister RNG
#

use English;
use Crypto::MT19937 qw/:all/;

my $seed = 719637;
&seed( $seed );

for (1 .. 1) {
    for (1 .. 10) {
	printf "%s ", &extract_number();
    }
    print "\n";
}

