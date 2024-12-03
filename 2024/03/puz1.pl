#!/usr/bin/perl
require 5;
use warnings;

my $sum = 0;

while (<>) {
  while (s{mul\((\d{1,3}),(\d{1,3})\)}{}) {
    #print "$1 * $2\n";
    $sum += $1*$2;
  }
}

print "$sum\n";
