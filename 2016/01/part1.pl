#!/usr/bin/perl
require 5;
use warnings;
use strict;

my $x = 0;
my $y = 0;
my $dir = 0;
my $half_pi = atan2 1, 0;

while (<<>>) {
  chomp;
  while ($_) {
    while (s/^L//) { $dir = ($dir-1) % 4 }
    while (s/^R//) { $dir = ($dir+1) % 4 }
    while (s/^(\d+)//) {
      $x += $1 * int(cos($half_pi*$dir));
      $y += $1 * int(sin($half_pi*$dir));
    }
    s/^,\s*//;
  }
}
printf "($x,$y)\t|$x| + |$y| = %d\n", abs($x)+abs($y);
