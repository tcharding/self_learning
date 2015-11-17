package LivingCreature;

use 5.022000;
use strict;
use warnings;


sub speak {
    my $class = shift;
    print "a $class goes ", $class->sound, "!\n";
}


1;
