#!/usr/bin/perl

require 5;
use warnings;
use strict;

use feature 'signatures';

my @ranges;

# Compares two ranges, each of which is a listref with length 2.  If a range is
# fully less than or completely greater than another range, returns -1 or 1
# respectively.  If it's identical, returns 0.  Otherwise, there is some
# overlap.  In that case, returns a listref representing the combined range.
sub compare ($a, $b) {
  #print "c";
  return -1 if $a->[1] < $b->[0];
  return 1 if $a->[0] > $b->[1];
  return 0 if $a->[0] == $b->[0] and $a->[1] == $b->[1];

  # Now, there are four cases.
  # 1. A completely encloses B.
  # 2. B completely encloses A.
  # 3. A overlaps B on the left.
  # 4. B overlaps A on the left.

  return $a if $a->[0] <= $b->[0] and $a->[1] >= $b->[1];
  return $b if $b->[0] <= $a->[0] and $b->[1] >= $a->[1];

  if ($a->[0] <= $b->[0]) { # A overlaps B on the left
    return [$a->[0], $b->[1]];
  } else {
    return [$b->[0], $a->[1]];
  }
}

sub insert ($a) {
  #print ".\n";
  for (my $i = 0; $i <= $#ranges; ++$i) {
    #print "i=$i\n";
    my $b = $ranges[$i];
    my $cmp = compare($a, $b);
    if ($cmp == -1) {
      next;
    } elsif ($cmp == 1) {
      splice @ranges, $i, 0, $a;
      return;
    } elsif ($cmp == 0) {
      return;
    } else {
      splice @ranges, $i, 1;
      return insert($cmp);
    }
  }
  push @ranges, $a;
}

while (<>) {
  chomp;
  last if /^\s*$/;
  die unless /^(\d+)-(\d+)$/;
  my($lo, $hi) = ($1, $2);
  insert([$lo, $hi]);
  if (0) {
    for (@ranges) {
      print "@{$_}\n";
    }
    print "--\n";
  }
}

my $tot = 0;
foreach my $range (@ranges) {
  $tot += $range->[1]-$range->[0]+1;
}

print "$tot\n";
