#!/usr/bin/perl -w
use strict;

$^I = ".bak";

while (<>) {
    s/(#!.*)/$1\n## Copyright (C) 2015 T-Bone/g;
    print;
}
