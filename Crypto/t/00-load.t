#!perl -T
use strict;
use warnings;

use Test::More;

BEGIN { use_ok('Crypto::Base') };
BEGIN { use_ok('Crypto::Vigenere') };
BEGIN { use_ok('Crypto::Block') };
BEGIN { use_ok('Crypto::Stream') };
BEGIN { use_ok('Crypto::Cipher') };
BEGIN { use_ok('Crypto::MT19937') };

done_testing();
