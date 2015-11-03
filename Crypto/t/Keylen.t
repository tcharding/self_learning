#!perl -T
use strict;
use warnings;

use Test::More;
use Crypto::Util qw(decode_ascii);
use Crypto::Keylen qw(:all);
use Crypto::Test;

my $a = &decode_ascii("this is a test");
my $b = &decode_ascii("wokka wokka!!!");

my $ed = &edit_distance( $a, $b );

is( $ed, 37, 'edit distance' );

# test transpose
 # my $input = decode_ascii("abcabcabc");
 # my $trans_blocks = &transpose($input, 3);
 # diag("input bits\n");
 # diag_bits($input);
 # diag("-----------------------------------------------------------\n");
 # diag("transposed bits (3 blocks)\n");
 # for (@$trans_blocks) {
 #     diag_bits($_);
 #     diag("---");
 # }

done_testing();
