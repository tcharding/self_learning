#!/usr/bin/perl -w
use strict;

while (<>) {
    if(/[A-Z][a-z]+\p{Space}.*/) {
	print $_;
    }
}
