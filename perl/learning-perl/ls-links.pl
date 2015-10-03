#!/usr/bin/perl -w
use strict;

for (<.* *>) {
    next unless (-l $_);
    printf "%s -> %s\n", $_, readlink $_;
}
