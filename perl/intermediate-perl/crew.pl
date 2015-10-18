#!/usr/bin/perl
use strict;
use warnings;

# crew.pl - data structure manipulation for crew members
#
# Tobin Harding

# items per person
my @skipper = qw(blue_shirt hat jacket sunscreen);
my @gilligan = qw(t_shirt);
my @professor = qw(shirt book hat);

my %crew = (
    Gilligan => \@gilligan,
    Skipper => \@skipper,
    Professor => \@professor,
);

check_items_for_all(\%crew);

sub check_items_for_all {
    my $crew = shift;		# hash reference
    for my $person (keys %$crew) {
	add_required_items ($person, $$crew{$person});
    }
    for my $person (keys %$crew) {
	print_person($person, $$crew{$person});
    }
}

sub print_person {
    my ($name, $items) = @_;

    printf "Name: $name\n";
    print map { " $_" } @$items;
    print "\n";
}

sub add_required_items {
    my ($who, $items) = @_;
    my %whose_items = map { $ _, 1 } @$items;

    my @required = qw(preserver water_bottle jacket hat);
    my @missing = ();

    for my $item (@required) {
	unless ($whose_items{$item}) {
	    print "$who is missing $item.\n";
	    push @missing, $item;
	}
    }

    if (@missing) {
#	print "Adding @missing to @$items for $who.\n";
	push @$items, @missing;
    }
}

