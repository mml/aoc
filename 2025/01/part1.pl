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
  $x += $dx;
  $x %= 100;
  ++$n if 0 == $x;
}
print "$n\n";
