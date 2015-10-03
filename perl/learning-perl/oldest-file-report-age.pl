#!/usr/bin/perl -w
use strict;

die "Usage: oldest-file.pl file [file ... ]\n"
    if (@ARGV == 0);

my $age = 0;
my $oldest_file;
my $oldest_age;

for (@ARGV) {
    my $this_age = -M;
    if ($this_age > $age) {
	$oldest_file = $_;
	$oldest_age = $this_age;
    }
}
printf "Oldest file: %s\t age: %d days\n", $oldest_file, $oldest_age;
