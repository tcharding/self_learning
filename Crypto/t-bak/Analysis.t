#!perl -T
use strict;
use warnings;

use Test::More;
use Crypto::Util qw(:all);
use Crypto::Analysis qw(:all);

my $m = decode_ascii("TOP SECRET: This is the plaintext");
#diag("m: $m\n");
my $k = byte_val('c');
#diag("k: $k\n");

my $c = &repeating_xor( $m, $k );
#diag("c: $c\n");
ok( $c );

my $scx = &bruteforce_scx( $c );
&rate_msgs( $scx );
my $top_rated = &get_top_rated( $scx );
my $n = @$top_rated;
if ($n == 1) {
    my $m = pop @$top_rated;
    my $p = encode_ascii( $m );
    diag( "\'$$scx{$m}{key}\' ($$scx{$m}{rating}) $p\n");
} else {
    diag("Unable to determine unique plain text message\n");
}

# cut and paste from chal-3.pl

my $input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736";
$c = decode_hex( $input );

my %scx;
#my $scx = &bruteforce_scx( $c );
&bruteforce_scx( $c, \%scx );
&rate_msgs( \%scx );

$top_rated = &get_top_rated( \%scx );

my $num = @$top_rated;
if ( $num != 1) {
    diag( "Cannot determine top rated plain text, returned ($num) results" );
}

for my $m (@$top_rated) {
    my $msg = encode_ascii( $m );
    diag( "\t\tok  # $scx{ $m }{key} ($scx{ $m }{ rating }) $msg (chal-3.pl)" );
}

done_testing();

