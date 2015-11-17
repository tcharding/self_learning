package Animal;
use parent qw(LivingCreature);

use 5.022000;
use strict;
use warnings;

sub speak {
    my $class = shift;
    print "a $class goes ", $class->sound, "!\n";
}

sub sound {
    die 'You must define sound in subclass';
}
1;
