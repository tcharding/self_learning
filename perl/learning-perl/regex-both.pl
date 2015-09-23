#!/usr/bin/perl -w
use strict;

while (<>) {
    if(/fred/) {
	    if(/wilma/) {
		print $_;
	    }
    }
}
