#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use Animal;
use Sheep;
use Cow;
use Mouse;
use Horse;
use Person;

my @creatures = qw(Cow Horse Sheep Mouse Person);
for my $beast (@creatures) {
    $beast->speak;
}

