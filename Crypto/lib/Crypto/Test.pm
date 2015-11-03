package Crypto::Test;

use 5.022000;
use strict;
use warnings;
use Test::More;

require Exporter;

our @ISA = qw(Exporter);

our @EXPORT = qw(
	diag_bits
);

our $VERSION = '0.01';

sub diag_bits {
    for (@_) {
	my $bits = $_;
	while (length($bits) > 0) {
	    my $byte = substr($bits, 0, 8);
	    $bits = substr($bits, 8);
	    diag("$byte ");
	}
	print "\n";
    }
}

1;
