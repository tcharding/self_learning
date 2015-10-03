#!/usr/bin/perl -w
use strict;

chomp (my $line = <STDIN>);
my @nums = split / /, $line;
my @sorted = sort {$a <=> $b} @nums;

for (@sorted) {
    printf "%10s\n", $_;
}
print "\n";
