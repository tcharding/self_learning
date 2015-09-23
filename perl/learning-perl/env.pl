#!/usr/bin/perl -w
use strict;

sub max_env {
    my @k = keys %ENV;
    my $max = 0;
    foreach (@k) {
	if ((length $_) > $max) {
	    $max = length $_;
	}
    }
    $max;
}

my $length = &max_env;

foreach (sort keys %ENV) {
    printf "%-${length}s %s\n", $_, $ENV{$_};
}
