#!/usr/bin/perl -w
use diagnostics;

$pi = 3.141592654;
print "Circumference Calculator\n";
print "========================\n";
print "\n";
print "Enter radius: ";
chomp($radius = <STDIN>);
if ($radius < 0) {
    $radius = 0;
}
print 2 * $pi * $radius . "\n";
 
