#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use Crypto::Util;

my $file = "trans.txt";
open my $fh, '<', $file or die "Cannot open file: $file\n";

my $input;
while ( <$fh> ) {
    chomp;
    $input .= $_;
}
my $c = decode_hex( $input );
dump_bits( $c );
&build_transposed( $c );

sub dump_bits {
    my $bits = shift;
    while( length( $bits )) {
	print substr( $bits, 0, 8);
	print " ";
	$bits = substr( $bits, 8);
    }
    print "\n";
}
sub build_transposed {
    my $input = shift;
    my $key_nchars = 3;
    my $key_nbits = $key_nchars * 8;
    my @blocks;
    my $keystr;			# 'passphrase' we are looking for
    my $clen = length( $c );
    for (my $bi = 0; $bi < $clen; $bi += 8) {
	my $ci = ($bi / 8) % $key_nchars;
	$blocks[$ci] .= substr( $c, $bi, 8);
    }
    for (@blocks) {
	dump_bits( $_ );
    }
}
