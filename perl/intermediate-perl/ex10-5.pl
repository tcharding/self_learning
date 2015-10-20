#!/usr/bin/perl
use strict;
use warnings;
use File::Basename;
use File::Spec::Functions;
use Data::Printer;

my $dir = '/home/tobin/Downloads';

die "Usage: $0 < breadth | depth > threshold\n"
    if (@ARGV < 2);

my $opt = $ARGV[0];
unless (($opt eq 'depth') || ($opt eq 'breadth')) {
    die "Unknown option: $opt\n";
}
my $threshold = $ARGV[1];
my $data = data_for_path($dir, $threshold, $opt);

sub data_for_path {
    my( $path, $threshold, $bod ) = @_;
    
    my $data = {};
    my @queue = ( [ $path, 0, $data ] );

    while ( my $next = shift @queue ) {
	my( $path, $level, $ref ) = @$next;

	my $basename = basename( $path );

	$ref->{$basename} = do {
	    if ( -f $path or -l $path ) { undef }
	    else {
		my $hash = {};
		if ( $level < $threshold ) {
		    opendir my ($dh), $path;
		    my @new_paths = map {
			catfile( $path, $_ )
		    } grep { ! /^\.\.?\z/ } readdir $dh;
		    if ($bod eq 'depth') {
			unshift @queue, map { [ $_, $level + 1, $hash ] } @new_paths;
		    } else {
			push @queue, map { [ $_, $level + 1, $hash ] } @new_paths;
		    }
		}
		$hash;
	    }
	};
    }
    $data;
}

p $data;
