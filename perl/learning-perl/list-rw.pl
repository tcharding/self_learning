#!/usr/bin/perl -w
use strict;

# return hash of stat($_)
sub stat_hash {
    my ($dev, $ino, $mode, $nlink, $uid, $gid, $rdev,
	$atim, $mtie, $ctime, $blksize, $blocks) = stat($_);
    my %stat_hash = (
	dev => $dev,
	dev => $dev,
	ino => $ino,
	mode => $mode,
	nlink => $nlink,
	uid => $uid,
	gid => $gid,
	rdev => $rdev,
	atim => $atim,
	mtie => $mtie,
	ctime => $ctime,
	blksize => $blksize,
	blocks => $blocks,
    );
    %stat_hash;
}

# return true if file is readable, writable and mine
sub attributes {
    my ($file) = @_;
    my $name = "tobin";
    my $uid = getpwnam($name);
    my %stat = &stat_hash($file);
    if ($stat{uid} == $uid) {
	printf "file %s belongs to %s", $file, $name;
	if (-r -w $file) {
	    print " and it has user rw permissions set\n";
	}
    }
}


die "Usage: $0 file [file ... ]\n"
    if (@ARGV == 0);


for (@ARGV) {
    &attributes($_);
}
