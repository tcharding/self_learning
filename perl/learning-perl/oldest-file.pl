#!/usr/bin/perl -w
use strict;

die "Usage: oldest-file.pl file [file ... ]\n"
    if (@ARGV == 0);

my $age = 0;
my $oldest;
for (@ARGV) {
    if (-M > $age) {
	$oldest = $_;
    }
}
print "Oldest file: $oldest\n";
