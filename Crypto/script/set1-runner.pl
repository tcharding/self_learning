#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

my @quick = qw( chal-1.pl chal-2.pl chal-3.pl );

for (@quick) {
    do "script/$_";
    say "";
}
