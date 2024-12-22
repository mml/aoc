#!/usr/bin/perl
require 5;
use warnings;
use strict;

my $x = 0;
my $y = 0;
my $dir = 0;
my $half_pi = atan2 1, 0;
my %locs = ("0,0" => 1);

while (<<>>) {
  chomp;
  while ($_) {
    while (s/^L//) { $dir = ($dir-1) % 4 }
    while (s/^R//) { $dir = ($dir+1) % 4 }
    while (s/^(\d+)//) {
      for (1..$1) {
        $x += int(cos($half_pi*$dir));
        $y += int(sin($half_pi*$dir));
        my $loc = "$x,$y";
        if (exists $locs{$loc}) {
          printf "Here: $loc (= %d)\n", abs($x)+abs($y);
          exit;
        }
        $locs{$loc} = 1;
      }
    }
    s/^,\s*//;
  }
}
printf "($x,$y)\t|$x| + |$y| = %d\n", abs($x)+abs($y);
