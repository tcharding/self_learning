#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

#
# Implement PKCS#7 padding
#

use Crypto::Block qw( pad );

my $padded = &pad("YELLOW SUBMARINE", 20);
my $expected = "YELLOW SUBMARINE\x04\x04\x04\x04";

print "Set 2 Challenge 9: ";
if ( $padded eq $expected ) {
    print "Completed!\n";
} else {
    print "Failed\n";
    print "expected: $expected\n";
    print "We got:   $padded\n";
}

