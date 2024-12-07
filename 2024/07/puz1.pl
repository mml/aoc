#!/usr/bin/perl
require 5;
use warnings;
use strict;

use feature 'signatures';

sub ops ($x, $num) {
  my @rv;
  for (1..$num) {
    if ($x & 1) {
      push @rv, '+';
    } else {
      push @rv, '*';
    }
    $x >>= 1;
  }
  return @rv;
}

my $total = 0;
LINE: while (<>) {
  my($ans,$nums) = split /:\s*/;
  my @nums = split ' ', $nums;
  foreach my $ops (0..2**($#nums)-1) {
    my @ops = ops($ops, $#nums);
    my $x = $nums[0];
    for (my $i = 0; $i < $#nums; ++$i) {
      $x = "($x)$ops[$i]$nums[$i+1]";
    }
    #print "$ans =? $x\n";
    if (eval($x) == $ans) {
      $total += $ans;
      next LINE;
    }
  }
}
print "$total\n";
