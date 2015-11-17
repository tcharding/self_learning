package Person;
use parent qw(LivingCreature);

use 5.022000;
use strict;
use warnings;

sub speak {
    my( $class, $line) = @_;
    if (defined $line) {
	print "Person: $line\n";	
    } else {
	print "a $class ", $class->sound , " beautifully\n";
    }

}

sub sound { 'hummmms' };
    
1;
