#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

#
# Create the MT19937 stream cipher and break it
#

use Crypto::MT19337 qw/:all/;

my $key = int(rand(2**16));

# encrypt using prng as a key stream
sub encrypt_prng {
    
}

# decrypt using prng as a key stream
sub decrypt_prng {
    
}
