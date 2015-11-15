#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

my $hex = "01";
my $bin = pack('H*', $hex);
my $ascii = unpack('A*', $bin);
$bin = pack('A*', $ascii);
my $rinsed = unpack('H*', $bin);

if ($hex ne $rinsed) {
    print "put fail\n";
} else {
    print "put success\n";
}
