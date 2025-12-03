#!/usr/bin/perl

require 5;
use warnings;
use strict;

my $sum = 0;

LINE: while (<>) {
  my $pat = qr//;
  my @digits = ();
  DIGIT: for (my $digits = 12; $digits > 0; $digits--) {
    my $suffix = $digits-1;
    for (my $i = 9; $i > 0; $i--) {
      if ($suffix > 0) {
        next unless /${pat}.*${i}\d{$suffix}/;
      } else {
        next unless /${pat}.*${i}/;
      }
      $pat = qr/${pat}.*${i}/;
      push @digits, $i;
      next DIGIT;
    }
    die;
  }
  $sum += join '', @digits;
}

print "$sum\n";
