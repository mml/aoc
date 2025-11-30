#!/usr/bin/perl

require 5;

use warnings;
use strict;

my $x = 0;

while (<>) {
  chomp;
  my $n = $_;
  while (1) {
    $n = int($n/3) - 2;
    last if $n <= 0;
    $x += $n;
  }
}

print "$x\n";
