#/usr/bin/perl -w
use strict;

# return hash of stat($_)
sub stat_hash {
    my ($dev, $ino, $mode, $nlink, $uid, $gid, $rdev,
	$atim, $mtie, $ctime, $blksize, $blocks) = stat($_);
    my %stat_hash = (
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
  
