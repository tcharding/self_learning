#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;


my $s = "this is a string";
my $bin = pack('A*', $s);
my $t = unpack('A*', $bin);
print "$t\n";

my $var;

for (1..3) {
    if ($var) {
	$var .= $_;
    } else {
	$var = $_;
    }
}
print "$var\n";
