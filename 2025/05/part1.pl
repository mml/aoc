#!/usr/bin/perl

require 5;
use warnings;
use strict;

use feature 'signatures';

my @lo;
my @hi;

while (<>) {
  chomp;
  last if /^\s*$/;
  die unless /^(\d+)-(\d+)$/;
  my($lo, $hi) = ($1, $2);
  push @lo, $lo;
  push @hi, $hi;
}

my $n = 0;

NUM: while (<>) {
  chomp;
  die unless /^\d+$/;
  for (my $i = 0; $i <= $#lo; ++$i) {
    if ($lo[$i] <= $_ and $_ <= $hi[$i]) {
      ++$n;
      next NUM;
    }
  }
}

print "$n\n";
