#!perl -T
use strict;
use warnings;

use Test::More;
use Crypto::Block qw/:all/;

my $s = "YELLOW SUB";
ok(my $padded = pad_pkcs_7( $s , 16));
#diag( "len: %s s: %s\n", length($padded), $padded );

$s = "this is a message";	# 17 characters
my $pblocks = &split_into_blocks( $s, 3 );
for (@$pblocks) {
    is(length( $_ ), 3);
}
is(@$pblocks, 6, "plaintext split into blocks and padded");
done_testing();
