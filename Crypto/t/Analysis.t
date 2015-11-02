#!perl -T
use strict;
use warnings;

use Test::More;
use Crypto::Util qw(:all);
use Crypto::Analysis qw(:all);

my $m = decode_ascii("TOP SECRET: This is the plaintext");
#diag("m: $m\n");
my $k = byte_val('c');
#diag("k: $k\n");

my $c = &repeating_xor( $m, $k );
#diag("c: $c\n");
ok( $c );

my $scx = &bruteforce_scx( $c );
&rate_msgs( $scx );
my $top_rated = &get_top_rated( $scx );
my $n = @$top_rated;
if ($n == 1) {
    my $m = pop @$top_rated;
    my $p = encode_ascii( $m );
    diag( "\'$$scx{$m}{key}\' ($$scx{$m}{rating}) $p\n");
} else {
    diag("Unable to determine unique plain text message\n");
}

done_testing();

