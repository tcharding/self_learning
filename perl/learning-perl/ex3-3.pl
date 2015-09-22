#!/usr/bin/perl -w

chomp (@lines = <STDIN>);
@sorted = sort @lines;
foreach (@sorted) {
    print "$_, ";
}
print "\n";

    
