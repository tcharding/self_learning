use strict;
use warnings;

use Test::More;
use Crypto::MT19937 qw/:all/;
use English;

my $seed = 630975;

my @res;
my $elements = 3;

for ( 1 .. 3 ) {
    my @tmp;
    &seed( $seed );
    for (0 .. $elements ) {
	push @tmp, &extract_number();
    }
    push @res, \@tmp;
}
for (0 .. $elements - 1) {
    my $n = ${$res[0]}[$_];
    is($n,  ${$res[1]}[$_], "res1");
    is($n,  ${$res[2]}[$_], "res2");
}

done_testing();

