#!/usr/bin/perl -w
use strict;

die "Usage: $0 <string> <substring>\n"
    if (@ARGV != 2);

my ($str, $sub) = @ARGV;
my $i = -1;
while (($i = index($str, $sub, $i+1)) != -1) {
    printf "%d ", $i;
}
print "\n";

