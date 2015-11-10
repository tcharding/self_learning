#!perl -T
use strict;
use warnings;

use Test::More;

BEGIN { use_ok('Crypto::Base') };
BEGIN { use_ok('Crypto::Vigenere') };

done_testing();
