#!/usr/bin/perl
use warnings;
use autodie;
use strict;

my @rev;
while (<>) {
    push(@rev, $_);
}
print reverse @rev;
