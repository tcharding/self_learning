#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use lib qw(/home/tobin/build/github/self_learning/Crypto/lib);
use Crypto::Util qw( decode_hex encode_ascii );
use Crypto::Analysis qw(:all);
use Data::Dumper;

my $input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736";
my $c = decode_hex( $input );
say "Set 1 Challenge 3, performing cryptanalysis on ciphertext ...";

my %scx;
#my $scx = &bruteforce_scx( $c );
&bruteforce_scx( $c, \%scx );
&rate_msgs( \%scx );

my $top_rated = &get_top_rated( \%scx );

my $num = @$top_rated;
if ( $num != 1) {
    say "Cannot determine top rated plain text, returned ($num) results";
}

for my $m (@$top_rated) {
    my $msg = encode_ascii( $m );
    print "$scx{ $m }{key} ($scx{ $m }{ rating }) $msg\n";
}


