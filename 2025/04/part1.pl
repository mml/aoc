#!/usr/bin/perl

require 5;
use warnings;
use strict;

my @rows;

while (<>) {
  chomp;
  push @rows, [split '', $_];
}

sub adj_occupied {
  my($x,$y) = @_;
  my $sum = 0;

  foreach my $dy (-1, 0, 1) {
    my $y = $y + $dy;
    next if $y < 0 or $y > $#rows;
    my @row = @{$rows[$y]};
    foreach my $dx (-1, 0, 1) {
      next if $dx == 0 and $dy == 0;
      my $x = $x + $dx;
      next if $x < 0 or $x > $#row;
      ++$sum if $row[$x] eq '@';
    }
  }
  return $sum;
}

my $tot;

for (my $y = 0; $y <= $#rows; ++$y) {
  my $row = $rows[$y];
  for (my $x = 0; $x <= $#$row; ++$x) {
    if ($row->[$x] eq '@' and adj_occupied($x,$y) < 4) {
      ++$tot;
      print 'x';
    } else {
      print $row->[$x];
    }
  }
  print "\n";
}

print "$tot\n";
