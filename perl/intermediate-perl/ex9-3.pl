#!/usr/bin/perl
use Data::Dumper;
use Regexp::Assemble;
use strict;
use warnings;
use autodie;


my $ra = Regexp::Assemble->new;
my $file = "regex.txt";
open my $fh, '<', $file;

while (<$fh>) {
    chomp $_;
    $ra->add("$_");
}
close $fh;
print $ra->re;

print "Enter lines to match regex\n";
while (<>) {
    if ($_ =~ $ra->re) {
#	my $match = substr($_, ;
	print "line: $. $_";
    }
}
