#!/usr/bin/perl
use Data::Dumper;
use strict;
use warnings;
use autodie;

my $file = "regex.txt";

my @patterns;

open my $fh, '<', $file;

while (<$fh>) {
    chomp $_;
    push @patterns, qr/$_/;
}
close $fh;

print Dumper(\@patterns);

print "Enter lines to match regex\n";
while (<>) {
    if ($_ ~~ @patterns) {
#	my $match = substr($_, ;
	print "line: $. $_ \n";
    }
}
