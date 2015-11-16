use strict;
use warnings;

use Test::More;
BEGIN { use_ok('Crypto::Base') };

my $string = "abc";
my $hex = "616263";
my $bh = pack('H*', $hex);
my $bs = pack('A*', $string);

is($bh, $bs, "check binary same");

is(unpack('H*', $bs), unpack('H*', $bh), "check unpack");

done_testing();
