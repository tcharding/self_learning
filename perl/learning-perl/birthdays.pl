#!/usr/bin/perl -w
use strict;
use DateTime;

#
# Globals
#
my $option; 			# command line option
my $person;			# command line arg: NAME
my %birthday = (
    Tobin => '15 04 1981',
    Amanda => '01 10 1972',
    Mya => '19 10 2002',
    Milla => '24 12 2009',
    Taivas => '27 11 2010',
    Charles => '17 12 2012',
);

#
# Subroutines
#
sub help {
    print "Usage: birthdays [OPTION] [NAME]\n";
    print "Display birthdate and age information. ";
    print "If NAME is provided, show only information for that person else display all.\n";
    print "\n";
    print "\t -d \t show birth day\n";
    print "\t -a \t show age\n";
    print "\t -h \t show this help\n";
}
# return maximum length of strings in argument
sub max_len {
    my $max = 0;
    foreach (@_) {
	if ((length $_) > $max) {
	    $max = length $_;
	}
    }
    $max;
}

sub birthday {
    my ($person) = @_;
    
    unless (defined $birthday{$person}) {
	print "$person does not exist, did you spell it right?\n";
	exit;
    }
    my $length = &max_len(keys %birthday);
    (my $day, my $month, my $year) = split / /, $birthday{$person};
    printf "-${length}s %s/%s/%s\n", $person, $day, $month, $year;
}

sub age {
    my ($person) = @_;
    
    unless (defined $birthday{$person}) {
	print "$person does not exist, did you spell it right?\n";
	exit;
    }
    (my $day, my $month, my $year) = split / /, $birthday{$person};

    my $now = DateTime->now;
    my $past = DateTime->new(
 	day => $day,
	month => $month,
	year => $year,
    );
    my $duration = $now - $past;
    my @units = $duration->in_units( qw(years months days) );
    my $length = &max_len(keys %birthday);
    printf "%-${length}s %d years, %d months, %d days\n", $person, @units;
}
#
# Main 
#

# Parse command line arguments
if (@ARGV == 0) {
    &help;
    exit 0;
} elsif (@ARGV == 1) {
    $option = $ARGV[0];
} elsif (@ARGV == 2) {
    $option = $ARGV[0];
    $person = $ARGV[1];
}

if ($option eq "-h") {
    &help;
} elsif ($option eq "-d") {
    if (defined $person) {
	&birthday($person);
    } else {
	foreach (sort keys %birthday) {
	    &birthday($_);
	}
    }

} elsif ($option eq "-a") {
    if (defined $person) {
	&age($person);
    } else {
	foreach (sort keys %birthday) {
	    &age($_);
	}
    }
}
