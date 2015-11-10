#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

#
# Detect AES in ECB mode
#

use Crypto::Block qw(has_repeating_blocks);

my $file = "script/8.txt";
open my $fh, '<', $file or die "Cannot open file: $file $!";

my @maybe;			# ECB candidates
while  (<$fh>) {
    chomp;
    if (has_repeating_blocks( $_, 16 )) {
	push @maybe, $_;
    }
}

my $num = @maybe;
print "Set 1 Challenge 8: ";
if ($num == 1) {
    print "Completed!\n";
} else {
    print "Failed: We got $num maybes\n";
}



