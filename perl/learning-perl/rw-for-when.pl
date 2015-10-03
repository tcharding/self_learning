#!/usr/bin/perl -w
use strict;
use 5.01001;

foreach (@ARGV) {
    print "file: $_ [ "
    when (-r _) { print "read "; continue }
    when (-w _) { print "write "; continue }
    when (-x _) { print "execute "; continue }

}
