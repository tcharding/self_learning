#!/usr/bin/perl -w
use strict;

sub attributes {
    my ($file) = @_;
    if (-e $file) {
	print "$file exists";
	print ", and is readable"
	    if -r $file;
	print ", and is writable"
	    if -w $file;
	print ", and is executable"
	    if -x $file;
    } else {
	print "$file does not exist\n";
    }
    print "\n";
}

foreach (@ARGV) {
    &attributes($_);
}
