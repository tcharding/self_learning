# Before 'make install' is performed this script should be runnable with
# 'make test'. After 'make install' it should work as 'perl My-List-Util.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use strict;
use warnings;

use Test::More tests => 5;
BEGIN { use_ok('My::List::Util') };

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

use My::List::Util qw/:all/;

# test sum
my @a = qw/ 1 2 3/;
my $res = sum( @a );
is( $res, 6, 'simple sum');
@a = (1, 2, 3);
is( sum(@a), 6, 'simple sum 2');
is( sum(qw/1/), 1);
is( sum(qw//), 0);

# test shuffle
my @list = qw/ 1 2 3 4 5 6 7 /;
for (1..10) {
   my @shuffled = shuffle( @list );
   diag("(@shuffled)\n");
}
