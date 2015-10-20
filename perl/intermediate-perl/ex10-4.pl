#!/usr/bin/perl
use strict;
use warnings;

use File::Basename;
use Data::Printer;

#my $dir = '/home/tobin/build/github/self_learning/perl';
my $dir = '/home/tobin/Downloads';
#dump_data_for_path( [ $dir, &data_for_path($dir) ] );
#exit;

my $ds = {};
my @queue = ( [ $dir, $ds ] );

while ( my $tuple = shift @queue ) {
    opendir my $dh, $tuple->[0] or do { warn "Skipping $tuple->[0]: $!\n"; next };
    foreach my $file ( grep { ! /\A\.\.?\z/ } readdir $dh ) {	
	next if -l $file;
	unless( -d "$tuple->[0]/$file" ) {
	    $tuple->[1]{$file} = undef;
	    next;
	}
	push @queue, [ "$tuple->[0]/$file", $tuple->[1]{$file} = {} ];
    }
}

dump_data_for_path($dir, $ds);

sub data_for_path {
    my $path = shift;
    if (-f $path or -l $path) {
	return undef;
    }
    if (-d $path) {
	my %directory;
	opendir PATH, $path or die "Cannot opendir $path: $!";
	my @names = readdir PATH;
	closedir PATH;
	for my $name (@names) {
	    next if $name eq '.' or $name eq '..';
	    $directory{$name} = data_for_path("$path/$name");
	}
	return %directory;
    }
    warn "$path is neither a file nor a directroy\n";
    return undef;
}

sub dump_data_for_path {
    my $path = shift;
    my $data = shift;
    my $level = shift || 0;

    my $basename = basename($path);
    print '  ' x $level, $basename;

    if (not defined $data) {
	print "\n";
	return;
    }
    if (keys %$data) {
	print ", with contets of:\n";
	foreach (sort keys %$data) {
	    dump_data_for_path("$path/$_", $data->{$_}, $level + 1);
	}
    }else {
	print ", an empty directory\n";
    }
}
