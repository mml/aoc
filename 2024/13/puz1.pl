#!/usr/bin/perl
require 5;
use warnings FATAL => qw(all);
use strict;

my $a_cost = 3;
my $b_cost = 1;

my %x;
my %y;
my $total_cost = 0;

while (<>) {
  chomp;
  if (/Button (.): X\+(\d+), Y\+(\d+)/) {
    $x{$1} = $2;
    $y{$1} = $3;
  } elsif (/Prize: X=(\d+), Y=(\d+)/) {
    print "$_\n";
    my $px = $1;
    my $py = $2;
    my $min_cost = 999_999_999;
    for (my $a = 0; $a < 100; ++$a) {
      for (my $b = 0; $b < 100; ++$b) {
        if ($a * $x{A} + $b * $x{B} == $px
            and $a * $y{A} + $b * $y{B} == $py) {
          my $cost = $a * $a_cost + $b * $b_cost;
          printf "%3d A=$a B=$b $1\n", $cost;
          $min_cost = $cost if $cost < $min_cost;
        }
      }
    }
    $total_cost += $min_cost if $min_cost < 999_999_999;
  }
}

print "\n$total_cost\n";
