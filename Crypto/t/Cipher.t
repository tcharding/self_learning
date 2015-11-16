use strict;
use warnings;

use Test::More;
use Crypto::Cipher qw/:all/;
use Crypt::Rijndael;
use Crypto::Block qw/:all/;

my $string = "this is a strin";
$string = pad( $string, 16 );
my $key = "yellow submarine";
my $iv = "this is 16 bytes";

# encrypt/decrypt with our sub
my $co = encrypt_aes_cbc( $string, $key, $iv );
my $p = decrypt_aes_cbc( $co, $key, $iv );

is( $p, $string, "test encrypt/decrypt cbc mode");

#
# Test compatibility with other implementation (Rijndael refers to Crypt::Rijndael)
#
my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_CBC() );
$cipher->set_iv( $iv );

# encrypt/decrypt with Rijndael 
my $cr = $cipher->encrypt( $string );
$p = $cipher->decrypt( $cr );
is( $p, $string, "encrypt/decrypt rijndael");

# check ciphertexts match
#is( $co, $cr, "test ciphertext ours/rijndael");

# encrypt with Rijndael decyrpt with ours
$p = decrypt_aes_cbc( $cr, $key, $iv );
is( $p, $string, "encrypt rijndael, decrypt ours");

# encrypt with ours decrypt with Rijndael
$p = $cipher->decrypt( $co );
is( $p, $string, "encrypt ours, decrypt rijndael");

done_testing();
