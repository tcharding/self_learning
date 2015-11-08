#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use Lifeform::Animal;
use Lifeform::Horse;
#use Lifeform::Cow;
#use Lifeform::Sheep;
#use Lifeform::Mouse;

my @barnyard = (
    { type => 'Horse', name => 'Danny' },
    { type => 'Sheep', name => 'Tim' },
    { type => 'Cow', name => 'Daisy' },
    { type => 'Mouse', name => 'Tiny'  },
    { type => 'Sheep', name => 'Betty' },
    { type => 'Cow', name => 'Tod' },
);

#say "My barnyard contains:";
#print_barnyard( @barnyard );

&Lifeform::Horse::test;
Horse->test;
exit;

    
for my $animal (@barnyard) {
    $$animal{type}->test;
}

my @neighbour;

while (1) {
    say "Enter an animal to add it to your barnyard (enter when done)";
    my $type = <STDIN>;
    if ( $type eq "\n") {
	print_barnyard( @neighbour );
	exit;
    }
    say "Enter a name for your $type";
    my $name = <STDIN>;
    push @neighbour, { type => $type, name => $name };
}

sub print_barnyard {
    for (@_) {
	my $type = ${$_}{type};
    }
}

