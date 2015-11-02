#!perl -T
use strict;
use warnings;

use Test::More;
use Crypto::Keylen qw(:all);
use Crypto::Util qw(decode_ascii);

my $a = decode_ascii("this is a test");
my $b = decode_ascii("wokka wokka!!!");

my $ed = &edit_distance( $a, $b );

is( $ed, 37, 'edit distance' );

done_testing();
