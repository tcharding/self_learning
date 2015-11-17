use strict;
use warnings;

use Test::More;
use Crypto::Prng;

my $prng = Crypto::Prng->new();
$prng->seed(630);
my $num = $prng->extract_number();
is(($num > 0), 1, "test extract");
for (1 .. 624 + 1) {
    my $num = $prng->extract_number();  # force twist
}

done_testing();
