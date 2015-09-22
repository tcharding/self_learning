#!/usr/bin/perl -w

@names = qw(fred betty barney dino wilma pebbles bamm-bamm);

@nums = <STDIN>;
foreach (@nums) {
    print "$names[$_ - 1]\n";
}
