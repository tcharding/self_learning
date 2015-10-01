#!/usr/bin/perl -w
use strict;

$^I = ".out";

while (<>) {
    s/[fF]red/Larry/g;
    print;
}
