use strict;
use warnings;

use Test::More;
use Crypto::Block qw/:all/;
BEGIN { use_ok('Crypto::Block') };

# test &pad_pks_7
my $s;
my $padded;
my $blsz = 8;			# block size

$s = "thisist";
$padded = $s . chr(1) x 1;
is( pad($s, $blsz), $padded, "pad 1");

$s = "thisis";
$padded = $s . chr(2) x 2;
is( pad($s, $blsz), $padded, "pad 2");

$s = "thisi";
$padded = $s . chr(3) x 3;
is( pad($s, $blsz), $padded, "pad 3");

$s = "this";
$padded = $s . chr(4) x 4;
is( pad($s, $blsz), $padded, "pad 4");

$s = "thisthis";
$padded = $s . chr($blsz) x $blsz;
is( pad($s, $blsz), $padded, "pad $blsz");

# test strip_pad
my $stripped;
$s = "this";
$stripped = strip_padding( pad($s, $blsz), $blsz);
is( $stripped , $s, "strip 1" );

$s = "thisthis";
$stripped = strip_padding( pad($s, $blsz), $blsz);
is( $stripped , $s, "strip 2" );

$s = "thisis";
$stripped = strip_padding( pad($s, $blsz), $blsz);
is( $stripped , $s, "strip 3" );

# test is_pad_correct
my $char = "a";			# random character
for (0..8) {
    &check_pad_correct( 4 );    
}

my @wrong_byte;
my @wrong_num;
my @mixed;
				# wrong byte
push @wrong_byte, "abcdefc" . chr(2);
push @wrong_byte, "abcdefc" . chr(3);
push @wrong_byte, "abcdefc" . chr(4);
				# wrong number of bytes
push @wrong_num,"abcd" . chr(4) x 3;
push @wrong_num,"abcd" . chr(4) x 2;
push @wrong_num,"abcd" . chr(4) x 1;
				# mixed padding
#push @mixed,"abcdef" . chr(2) . chr(1);  correctly padded?
push @mixed,"abcdef" . chr(1) . chr(2);
push @mixed,"abcde" . chr(1) . chr(2) . chr(4);
push @mixed,"abcde" . chr(1) . chr(3) . chr(3);
push @mixed,"abcde" . chr(3) . chr(1) . chr(3);

for (@wrong_byte) {
    is( &is_pad_correct( $_, $blsz ), 0, "is_pad_correct wrong_byte");    
}
for (@wrong_num) {
    is( &is_pad_correct( $_, $blsz ), 0, "is_pad_correct wrong_num");    
}
for (@mixed) {
    is( &is_pad_correct( $_, $blsz ), 0, "is_pad_correct mixed");    
}


sub check_pad_correct {
    my $pad = shift;
    my $correct = $char x ($blsz - $pad) . chr($pad) x $pad;
    is( &is_pad_correct( $correct, $blsz ), 1, "is_pad_correct $pad");
    
}


done_testing();
