#!/usr/bin/perl -w
use strict;

while (<>) {
    if(/[fF]red +/) {
	print $_;
    }
}
