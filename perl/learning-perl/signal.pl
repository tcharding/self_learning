#!/usr/bin/perl -w
use strict;

my %signals;

sub sig_int {
    $signals{int}++;
    printf "We have been interupted %d times\n", $signals{int};
    exit;
}

sub sig_usr1 {
    $signals{usr1}++;
    printf "We have been interupted %d times\n", $signals{usr1};
}

$SIG{'INT'} = 'sig_int';
$SIG{'USR1'} = 'sig_usr1';

while (1) {
    sleep 1;
}
