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
    my $px = 10000000000000+$1;
    my $py = 10000000000000+$2;
    s/=/=10000000000000/g;
    print "$_\n";
    my $a = ($py*$x{B} - $px*$y{B})/($x{B}*$y{A} - $x{A}*$y{B});
    my $b = ($px-$a*$x{A})/$x{B};
    my $cost = $a_cost*$a + $b_cost*$b;
    printf "%20d A=$a B=$b\n", $cost;
    $total_cost += $cost if int($cost) == $cost;
    #$total_cost += $min_cost if $min_cost < 999_999_999;
  }
}

print "\n$total_cost\n";
