#!/usr/bin/perl -w
use strict;

die "Usage: $0 [-s] TARGET LINK_NAME\n"
    if (@ARGV < 2);

my $flag = 0;			# soft link flag
if ($ARGV[0] =~ /^\-+/) {
    my $option = $ARGV[0];
    if ($option eq "-s") {
	shift @ARGV;
	$flag = 1;
    } else {
	die "Unsupported option: $option\n";
    }
}

my ($target, $link_name) = @ARGV;
if ($flag == 1) {
    symlink $target, $link_name;    
} else {
    link $target, $link_name;    
}

