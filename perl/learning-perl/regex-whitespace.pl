#!/usr/bin/perl -w
use strict;

while (<>) {
    chomp;
    if (/\s\z/) {
	print "ends in whitespace\n";
    } else {
	print "does not end in whitespace\n";
    }
}
