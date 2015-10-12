#!/usr/bin/perl
use strict;
use warnings;
use local::lib;
use v5.14;
use Module::CoreList;

# ex-2.2.pl - print corelist report
#
# Taken from text book answers

my @modules = sort keys $Module::CoreList::version{5.014002};

my $max_length = 0;
foreach my $module ( @modules ) {
    $max_length = length $module if
	length $module > $max_length;
}

foreach my $module ( @modules ) {
    printf "%*s %s\n",
	- $max_length,
	$module,
	$Module::CoreList::released{
	  Module::CoreList->first_release( $module )
	};
}
