#!/usr/bin/perl -w
use strict;
use warnings;

my $a = "1c0111001f010100061a024b53535009181c";
my $b = "686974207468652062756c6c277320657965";
my $expected = "746865206b696420646f6e277420706c6179";

my $out = unpack('h*',pack('h*', $a) ^ pack('h*', $b));

print "Set 1 Challenge 2: ";
if ($out eq $expected) {
    print "Completed!\n";
} else {
    print "Failed\n";
    print "expected: $expected\n";
    print "We got:   $out\n";
}


