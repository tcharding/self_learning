#!/usr/bin/perl
use utf8;
use strict;
use warnings;
use Data::Dumper;

use v5.12;

my %patterns = (
    Gilligan   => qr/(?:Wiley )?Gilligan/,
    'Mary-Ann' => qr/Mary-Ann/,
    Ginger     => qr/Ginger/,
    Professor  => qr/(?:The )?Professor/,
    Skipper    => qr/Skipper/,
    'A Howell' => qr/Mrs?. Howell/,
);

# my $key = ans_rightmost(
#     'There is Mrs. Howell, Ginger, and Gilligan',
#     \%patterns
# );
# print "key: $key\n";

# my $position = &rightmost( 
#     'There is Mrs. Howell, Ginger, and Gilligan',
#     @patterns{ sort keys %patterns }
# );

my $key = &rightmost(
    'There is Mrs. Howell, Ginger, and Gilligan',
    \%patterns
);
print "name: $key\n";


sub rightmost {
    my( $string, $href ) = @_;
    my $matching = "not found";
    my $rightmost = -1;
    my @patterns = @{ $href }{keys %{ $href }};
    
    while ( my( $i, $pattern ) = each @patterns ) {
	my $position = $string =~ m/$pattern/ ? $-[0] : -1;
	if ($position > $rightmost) {
	    	$rightmost = $position;
		$matching = $pattern;
	}
    }
    for (keys %{ $href }) {
	if ($matching eq ${ $href }{$_}) {
	    return $_;
	}
    }
    return $matching;
}
