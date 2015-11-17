package LivingCreature;

use 5.022000;
use strict;
use warnings;


sub speak {
    my $either = shift;
    print $either->name,  " goes ", $either->sound, "!\n";
}


1;
