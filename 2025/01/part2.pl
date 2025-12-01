#!/usr/bin/perl

require 5;
use warnings;
use strict;

my $x = 50;
my $n = 0;
while (<>) {
  die unless /^(L|R)(\d+)$/;
  my $dx = $2;
  $dx *= -1 if $1 eq 'L';
  for (my $i = 1; $i <= $dx; ++$i) {
    $x++;
    $x %= 100;
    ++$n if 0 == $x;
  }
  for (my $i = $dx; $i < 0; ++$i) {
    $x--;
    $x %= 100;
    ++$n if 0 == $x;
  }
}
print "$n\n";
