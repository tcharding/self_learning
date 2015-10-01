#!/usr/bin/perl -w
use strict;
while (<>) {
    chomp;
    if(/(?<aword>.*a\b)(?<chars>.{0,5})/) {
	print "Matched: |$`<$&>$'|\n";
	print ("\'$+{aword}\' and the chars: \'$+{chars}\'\n");
    } else {
	print "No match: |$_|\n";
    }
}
