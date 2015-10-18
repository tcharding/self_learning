#!/usr/bin/perl
use strict;
use warnings;

my %gilligan_info = (
	name     => 'Gilligan', 
	hat      => 'White', 
	shirt    => 'Red', 
	position => 'First Mate',
	location => 'The Island',
);
my %skipper_info = (
	name     => 'Skipper',
	hat      => 'Black',
	shirt    => 'Blue',
	position => 'Captain',
	location => 'The Island',
);

my %mrs_howell_info = (
	name     => 'Mrs Howel',
	hat      => 'Black',
	shirt    => 'Red',
	position => 'Cook',
	location => 'The Island Country Club',    
);

my %mr_howell_info = (
	name     => 'Mr Howel',
	hat      => 'Black',
	shirt    => 'Blue',
	position => 'Garderer',
	location => 'The Island Country Club',    
);

my @crew = (\%gilligan_info, \%skipper_info, \%mrs_howell_info, \%mr_howell_info);
print_location(\@crew);

sub print_location {
    my $people = shift;

    for my $person (@$people) {
	printf "%s at %s\n", $person->{'name'}, $person->{location};
    }
}

#my $format = "%-15s %-7s %-7s %-15s\n"; 
#printf $format, qw(Name Shirt Hat Position); 

# foreach my $crewmember (@crew) {
# 	printf $format, 
# 		$crewmember->{'name'}, 
# 		$crewmember->{'shirt'}, 
# 		$crewmember->{'hat'}, 
# 		$crewmember->{'position'};
# }
