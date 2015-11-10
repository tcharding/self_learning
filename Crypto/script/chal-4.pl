#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

#
# Detect single-character XOR
#

use Crypto::Base qw( hex_to_ascii );
use Crypto::Vigenere qw(:all);

my $file = "script/4.txt";
open my $fh, '<', $file or die "Cannot open input file $file\n";
say "Set 1 Challenge 4, performing cryptonalysis on ciphertext: $file ...";

my @scxs;
while (<$fh>) {
    chomp;
    push @scxs, &bruteforce_scx( $_ );
}

for (@scxs) {
    &rate_msgs( $_ );
}

my $max;
my $first_time = 0;
for my $scx (@scxs) {
    for my $char ( keys %$scx ) {
	my $rating = $$scx{ $char }{ rating };
	if ($first_time == 0) {
	    $max = $rating;
	    $first_time = 1;
	} else {
	    $max = $rating if ( $rating > $max );	    
	}
    }
}
my( @top_rated, $hex );
for my $scx (@scxs) {
    for my $char ( keys %$scx ) {
	my $rating = $$scx{ $char }{ rating };
	if ( $rating == $max ) {
	    push @top_rated, $$scx{ $char }{ hex };
	}
    }
}
my $n = @top_rated;
if ($n == 1) {
    my $hex = pop @top_rated;
    print hex_to_ascii( $hex );
} else {
    say "Cannot determine top rated plain text, returned ($n) results";
}
