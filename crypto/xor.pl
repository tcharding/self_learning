#!/usr/bin/perl -w
use strict;

# xor same length bit strings
sub xor {
    my ($plain, $key) = @_;
    my $cypher;
    if (length($plain) != length($key)) {
	die "Strings should be same length\n";
    }
    while (length($plain) > 0) {
	my $p = substr($plain, 0, 1);
	$plain = substr($plain, 1);
	my $k = substr($key, 0, 1);
	$key = substr($key, 1);
	my $sum = $p + $k;	# add the two bits together
	if ($sum == 1) {
	    $cypher .= "1";
	} else {
	    $cypher .= "0";
	}
    }
    return $cypher;
}
