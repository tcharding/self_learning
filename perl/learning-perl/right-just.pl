#!/usr/bin/perl -w
use strict;
use autodie;

print "1234567890123456789012345678901234567890\n";
while (<STDIN>) {
    printf "%21s\n", $_;
}
    
