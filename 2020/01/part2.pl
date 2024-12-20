#!/usr/bin/perl
require 5;
use warnings;
use strict;

my @l;
LINE: while (<STDIN>) {
  chomp;
  my $n = 2020 - $_;
  foreach (my $i = 0; $i <= $#l; ++$i) {
    my $n = $n - $l[$i];
    if ($n > 0) {
      foreach (my $j = $i+1; $j <= $#l; ++$j) {
        last if ($l[$j] > $n);
        if ($l[$j] == $n) {
          printf "%d\n", $l[$i] * $_ * $l[$j];
          last LINE;
        }
      }
    }
  }
  @l = sort { $a <=> $b } (@l, $_);
}
