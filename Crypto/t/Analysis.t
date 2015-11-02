#!perl -T
use strict;
use warnings;

use Test::More;
use Crypto::Util qw(:all);
use Crypto::Analysis qw(:all);

my $m = decode_ascii("TOP SECRET: This is the plaintext");
my $k = byte_val('c');
#diag("$k\n");
my $c = &repeating_xor( $m, $k );
ok( $c );

my $scx = &bruteforce_scx( $c );
&rate_msgs( $scx );
my $top_rated = &get_top_rated( $scx );
my $n = @$top_rated;
if ($n == 1) {
    my $m = pop @$top_rated;
    my $p = encode_ascii( $m );
    my $k = encode_ascii( $$scx{$m}{key});
    my $rating = $$scx{$m}{rating};
    diag( "$k ($rating) $p\n");
} else {
    diag("Unable to determine unique plain text message\n");
}

done_testing();

