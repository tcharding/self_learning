#!/usr/bin/perl -w
use strict;
use Module::CoreList;
 
my %modules = %{ $Module::CoreList::version{5.014} };
my $max = 0;
foreach (keys %modules) {
  if ((length $_) > $max) {
    $max = length $_;
  }
}

foreach (sort keys %modules) {
  printf "%-${max}s %s\n", $_, $modules{$_};
}
