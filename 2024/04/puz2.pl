#!/usr/bin/perl
require 5;
use warnings;
use strict;

my @rows;
my $total = 0;

while (<>) {
  chomp;
  push @rows, $_;
}

for (my $i = 0; $i <= $#rows-2; ++$i) {
  for (my $j = 0; $j < length($rows[$i])-2; ++$j) {
    local $_ =
        substr($rows[$i], $j, 3) .
        substr($rows[$i+1], $j, 3) .
        substr($rows[$i+2], $j, 3);
    $total += /M.M.A.S.S|M.S.A.M.S|S.M.A.S.M|S.S.A.M.M/;
  }
}

print "$total\n";
