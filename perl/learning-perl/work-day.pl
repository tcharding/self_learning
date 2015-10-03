#!/usr/bin/perl -w
use strict;

my $today = `date`;
if (($today =~ /\ASat/) || ($today =~ /\ASun/)) {
    print "Go Play\n";
} else {
    print "Get to Work\n";
}
