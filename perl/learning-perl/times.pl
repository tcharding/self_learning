#!/usr/bin/perl -w
use strict;
use List::Util qw(maxstr);


sub max_len {
    my $max = 0;
    foreach (@_) {
	if ((length $_) > $max) {
	    $max = length $_;
	}
    }
    $max;
}

my @files = qw( times.pl );
my $max = &max_len(@files);

for (@files) {
    my ($atime, $mtime) = (stat $_)[8, 9];
    my @time = localtime $mtime;
    my $s = localtime $mtime;
    print "$s\n";
    print "@time\n";
}

