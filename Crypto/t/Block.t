use strict;
use warnings;

use Test::More;
use Crypto::Block qw/:all/;
BEGIN { use_ok('Crypto::Block') };

# test &pad_pks_7
my $s;
my $padded;

$s = "thisist";
$padded = $s . chr(1) x 1;
is( pad($s, 8), $padded, "pad 1");

$s = "thisis";
$padded = $s . chr(2) x 2;
is( pad($s, 8), $padded, "pad 2");

$s = "thisi";
$padded = $s . chr(3) x 3;
is( pad($s, 8), $padded, "pad 3");

$s = "this";
$padded = $s . chr(4) x 4;
is( pad($s, 8), $padded, "pad 4");

$s = "thisthis";
$padded = $s . chr(8) x 8;
is( pad($s, 8), $padded, "pad 8");

# test 
my $stripped;
$s = "this";
$stripped = strip_padding( pad($s, 8), 8);
is( $stripped , $s, "strip 1" );

$s = "thisthis";
$stripped = strip_padding( pad($s, 8), 8);
is( $stripped , $s, "strip 2" );

$s = "thisis";
$stripped = strip_padding( pad($s, 8), 8);
is( $stripped , $s, "strip 3" );


done_testing();
