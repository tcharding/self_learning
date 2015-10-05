#!/usr/bin/perl -w
use strict;
use autodie;
use File::Basename;
use File::Copy;

# mod-file.pl - copy soure file to exercise directory with modifications
#
# Tobin Harding

if (@ARGV != 3) {
    printf "%s\n%s\n%s\n%s\n",
	"Usage: $0 OPTION file ex-number",
	"OPTION",
	"\t -l \t lib file",
	"\t -e \t executable file";
    exit;
}

my $src_path;			# source file path name
my $src_file;			# source file base name
my $dst_dir;			# destination directory
my $dst_file;			# destination file name
my $dst_path;			# destination path name
my $num;			# exercise number (1.15)

# parse option and set mode
my $mode;
my $option = shift;
if ($option eq "-e") {
    $mode = "exe";
} else {
    $mode = "lib";
}

# parse arguments
($src_path, $num) = @ARGV;
$src_file = $src_path;
$src_file = basename $src_path;
#printf "Got\n src_path: %s\n src_file: %s\n num: %s\n", $src_path, $src_file, $num;

# Sanity checks
if (($mode ne "lib") && ($mode ne "exe")) {
    printf "Error: option unknown: %s (mode: %s)\n", $option, $mode;
}
if ( ! -e $src_path) {
    printf "Error: pathname does not appear to exist: %s\n", $src_path;
    exit;
}

if ( ! ($num =~ /\d+\.\d+/) ) {
    printf "Error: ill formatted num arg (expected form: '1.15'): %s\n", $num;
    exit;
}

my ($chnum, $exnum) = ($num =~ /(\d+)\.(\d+)/);
#printf "Got\n chnum: %s\n exnum: %s\n", $chnum, $exnum;
$dst_dir = "ex-ch$chnum";
if (! -e $dst_dir) {
    die "$dst_dir not drw: $!";   
}
$dst_file = $src_file;
$dst_file =~ s/(.*).c\Z/$1-ex-${num}.c/;
$dst_path = "$dst_dir/$dst_file";
#printf "dst_path: %s\n", $dst_path;

open my $src_fh, '<', "$src_path";
open my $dst_fh, '>', "$dst_path";

<$src_fh>;			# skip first line
printf $dst_fh "/\* orginal file: \.\./$src_path \*/";
while (<$src_fh>) {
    s/\s+/ /g;
    printf $dst_fh "%s\n", $_;
}

# Add rules to makefile
my $basename = $dst_file;
$basename =~ s/(.*)\.c/$1/;
#print "basename: $basename\n";
my $mkin = "$dst_dir/Makefile";
my $mkout = "$mkin.bak";
open my $mkin_fh, '<', $mkin;
open my $mkout_fh, '>', $mkout;

while (<$mkin_fh>) {
    # add file to SCRS
    s/\A(SRCS.*)/$1 ${basename}.c/;
    # add file to EXES
    s/\A(EXES.*)/$1 ${basename}/;
    # now add a makefile rule
    my $rule = "$basename: $basename.o\n\t\$(CC) -o \$\@ $basename.o \$(LFLAG)";
    s/(\Aall:.*)/$1\n\n$rule/;
    print $mkout_fh "$_";
}

close $mkout_fh;
close $mkin_fh;
rename $mkout, $mkin;
