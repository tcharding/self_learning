#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use lib qw(/home/tobin/build/github/self_learning/Crypto/lib);
use Crypto::Convert qw(:all);

my $file = "script/8.txt";
open my $fh, '<', $file or die "Cannot open file: $file $!";

my @maybe;			# ECB candidates
while (<$fh>) {
    chomp;
    if (has_repeats( $_ )) {
	push @maybe, $_;
    }
}

my $num = @maybe;
print "We got $num maybes\n";
if ($num == 1) {
    my $hex = pop @maybe;
    print "$hex\n";
}

sub has_repeats {
    my $hex = shift;
    my( $i, $j );
    my $chunk = 32;		# 32 hex digits = 16 bytes
    for( $i = 0; $i < length( $hex ); $i += $chunk ) {
	my $byte = substr( $hex, $i, $chunk );
	for( $j = $i + $chunk; $j < length( $hex ); $j += $chunk ) {
	    my $cmp = substr( $hex, $j, $chunk );
	    if( $byte eq $cmp ) {
		return 1;	# true
	    }
	}
    }
    return 0;			# false
}
