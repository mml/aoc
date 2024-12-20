#!/usr/bin/perl
require 5;
use warnings;
use strict;

my @l;
while (<STDIN>) {
  chomp;
  foreach (my $i = 0; $i <= $#l; ++$i) {
    if ($l[$i] + $_ == 2020) {
      printf "%d\n", $l[$i] * $_;
      last;
    }
  }
  push @l, $_;
}
