#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use RaceHorse;

my $nag = RaceHorse->new('Billy Boy');

# record the outcomes: 3 wins, 1 show, 1 loss
$nag->won;
$nag->won;
$nag->won;
$nag->showed;
$nag->lost;

print $nag->name, ' has standings of: ', $nag->standings, ".\n";
