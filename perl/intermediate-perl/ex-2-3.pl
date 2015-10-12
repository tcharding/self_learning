#!/usr/bin/perl
use strict;
use warnings;
use Business::ISBN;

# ex-2-3.pl - parse ISBN 
#
# Tobin Harding

my $isbn = Business::ISBN->new('978-1-449-39309-0');
my $group = $isbn->group_code;
my $publisher = $isbn->publisher_code;

printf "group: %s publisher: %s\n", $group, $publisher;

