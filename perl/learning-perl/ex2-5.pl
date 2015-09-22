#!/usr/bin/perl -w

print "Enter string: \n";
$string = <STDIN>;
print "Enter num: \n";
chomp ($num = <STDIN>);
$res = $string x $num;
print $res;
