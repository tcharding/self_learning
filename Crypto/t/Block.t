#!perl -T
use strict;
use warnings;

use Test::More;
use Crypto::Block qw/:all/;

my $s = "YELLOW SUB";
ok(my $padded = pad_pkcs_7( $s , 16));
#diag( "len: %s s: %s\n", length($padded), $padded );

$s = "this is a message";	# 17 characters
my $pblocks = &split_string_into_blocks( $s, 3 );
for (@$pblocks) {
    is(length( $_ ), 3);
}
is(@$pblocks, 6, "plaintext split into blocks and padded");

$s = "this";
$padded = &pad( $s, 8 );
is(length( $padded ), 8, "padding 4 to 8 bytes");
$s = strip_padding( $s );
is( (index $s, chr(0x04)), -1, "No padding present");

done_testing();
