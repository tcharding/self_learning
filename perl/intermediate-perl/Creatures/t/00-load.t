# Before 'make install' is performed this script should be runnable with
# 'make test'. After 'make install' it should work as 'perl Animal.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use strict;
use warnings;

use Test::More;
BEGIN { use_ok('LivingCreature') };
BEGIN { use_ok('Person') };
BEGIN { use_ok('Animal') };
BEGIN { use_ok('Cow') };
BEGIN { use_ok('Sheep') };
BEGIN { use_ok('Mouse') };
BEGIN { use_ok('Horse') };
BEGIN { use_ok('Barn') };
BEGIN { use_ok('RaceHorse') };

diag( "Testing Animal $Animal::VERSION, Perl $], $^X" );

done_testing();
#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

