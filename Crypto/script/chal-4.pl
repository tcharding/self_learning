#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use lib qw(/home/tobin/build/github/self_learning/Crypto/lib);
use Crypto::Util qw( decode_hex encode_ascii );
use Crypto::Analysis qw(:all);

my $file = "script/4.txt";
open my $fh, '<', $file or die "Cannot open input file $file\n";
say "Set 1 Challenge 4, performing cryptonalysis on ciphertext: $file ...";

my %scx;
while (<$fh>) {
    chomp;
    my $c = decode_hex( $_ );
     &bruteforce_scx( $c, \%scx );
}

&rate_msgs( \%scx );
my $top_rated = &get_top_rated( \%scx );

if ( $#{$top_rated} != 0) {
    say "Cannot determine top rated plain text, returned ($#{$top_rated}) results";
}

for my $m (@$top_rated) {
    my $msg = encode_ascii( $m );
    print "$scx{ $m }{key} ($scx{ $m }{ rating }) $msg\n";
}
