#!perl -T
use strict;
use warnings;

use Test::More;

BEGIN { use_ok('Crypto::Base') };
BEGIN { use_ok('Crypto::Vigenere') };
BEGIN { use_ok('Crypto::Block') };

done_testing();
