#!/usr/bin/perl

require 5;
use warnings;
use strict;

my $sum = 0;

LINE: while (<>) {
  for (my $i = 9; $i > 0; $i--) {
    next unless /${i}\d/;
    for (my $j = 9; $j > 0; $j--) {
      if (/${i}.*${j}/) {
        $sum += "${i}${j}";
        next LINE;
      }
    }
    die;
  }
}

print "$sum\n";
