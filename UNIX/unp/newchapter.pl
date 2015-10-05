#!/usr/bin/perl -w
use strict;
use autodie;
use File::Copy;

# newchapter.pl - set up new exercise chapter.
my $templates = "templates";
my $makefile = "${templates}/Makefile";
my $answerfile = "${templates}/answers.md";
my $link_name = "cur";

die "Usage: $0 <chapter number>\n"
    if (@ARGV != 1);

my $chnum = $ARGV[0];
my $dir = "ex-ch${chnum}";


if (-e $dir) {
    printf "File %s appears to already exist ... aborting\n", $dir;
    exit;
}

# create directory and copy templates 
mkdir $dir;
copy ($makefile, $dir) or die "Copy failed: $!";
copy ($answerfile, $dir) or die "Copy failed: $!";

# set up soft link
if (-e $link_name) {
    unlink $link_name;
}
symlink $dir, $link_name or die "Cannot create soft link: $!";
printf "New chapter created: %s (softlink: %s)\n", $dir, $link_name;
