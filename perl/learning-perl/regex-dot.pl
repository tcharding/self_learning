#!/usr/bin/perl -w
use strict;

while (<>) {
    if(/\.+/) {
	print $_;
    }
}
