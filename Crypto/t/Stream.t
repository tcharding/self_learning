use strict;
use warnings;

use Test::More;
BEGIN { use_ok('Crypto::Stream') };

use Crypto::Stream qw(:all);

my $key = "YELLOW SUBMARINE";
my $nonce = 0;
my $counter = 0;
my $p = "this is the message";
my $c = encrypt_aes_ctr( $p, $key, $nonce, $counter );
my $rinsed = decrypt_aes_ctr( $c, $key, $nonce, $counter );

is($p, $rinsed, "test encrypt/decrypt AES CTR mode");

done_testing();
