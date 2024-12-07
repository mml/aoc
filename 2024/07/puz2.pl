#!/usr/bin/perl
require 5;
use warnings;
use strict;

use feature 'signatures';

sub ops ($x, $num) {
  my @rv;
  for (1..$num) {
    my $lsd = ($x % 3);
    if ($lsd == 0) {
      push @rv, '+';
    } elsif ($lsd == 1) {
      push @rv, '*';
    } else {
      push @rv, '.';
    }
    $x = int($x/3);
  }
  return @rv;
}

my $total = 0;
my $line = 0;
$| = 1;
LINE: while (<>) {
  ++$line;
  print "$line\r";
  my($ans,$nums) = split /:\s*/;
  my @nums = split ' ', $nums;
  foreach my $ops (0..(3**($#nums))-1) {
    my @ops = ops($ops, $#nums);
    #print "ops($ops, $#nums) = =@ops\n";
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
