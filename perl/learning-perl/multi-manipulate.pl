#!/usr/bin/perl -w
use strict;

$^I = ".out";

while (<>) {
    s/[fF]red/ftmp/g;
    s/[wW]ilma/wtmp/g;
    s/wtmp/Fred/g;
    s/ftmp/Wilma/g;
    print;
}
