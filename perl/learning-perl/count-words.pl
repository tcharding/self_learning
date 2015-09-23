#!/usr/bin/perl -w
use strict;

my %count;

while (<>) {
    chomp $_;
    if (exists $count{$_}) {
#	print "incrementing word: $_\n";
	$count{$_} =  $count{$_} + 1;
    }
    else {
#	print "adding now word: $_\n";
	$count{$_} = 1;
    }
}
print "dumping hash [word: count]\n";
while ( (my $key, my $value) = each %count) {
    print ("$key => $value\n");
}
