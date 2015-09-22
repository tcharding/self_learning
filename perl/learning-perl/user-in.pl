#!/usr/bin/perl -w
use strict;
use autodie;

print "enter column width:";
chomp (my $column = <STDIN>);
print "column: $column\n";    
print "1234567890123456789012345678901234567890\n";
while (<STDIN>) {
    printf "%${column}s\n", $_;
}
    
