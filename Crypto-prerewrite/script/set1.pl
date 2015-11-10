#!/usr/bin/perl
use strict;
use warnings;

use feature qw/say/;

my @run = qw/ chal-1.pl chal-2.pl chal-3.pl chal-4.pl chal-5.pl chal-6.pl chal-7.pl /;
for ( @run ) {
    do "script/$_";
    say "";
}
