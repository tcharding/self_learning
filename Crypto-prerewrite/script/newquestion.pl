#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

die "Usage: $0 num\n" if @ARGV != 1;
my $num = shift;

my $template =
    "#!/usr/bin/perl\nuse strict;\nuse warnings;\nuse feature qw/say/;\n";

my $link = "script/cur";
if (-e $link) {
    unlink $link;
}

my $file = sprintf "chal-%s.pl", $num;
my $path = "script/$file";
open my $fh, '>', $path or die;

print $fh "$template";
close $fh;
symlink $file, $link;
