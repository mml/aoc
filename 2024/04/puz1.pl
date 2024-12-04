#!/usr/bin/perl
require 5;
use warnings;
use strict;

use feature 'signatures';

my $red = "\e[31m";
my $white = "\e[37m";
my $smul = "\e[4m";
my $rmul = "\e[24m";

my $total = 0;
my @rows;
my @columns;
my @left_diag;
my @right_diag;
my $y = 0;

sub count ($str) {
  my $n = 0;
  local $_ = $str;
  while (s/XMAS/..../) { ++$n }
  local $_ = $str;
  while (s/SAMX/..../) { ++$n }
  return $n;
}

sub hilite ($str) {
  local $_ = $str;
  s/XMAS/${red}XMAS${white}/g;
  s/(S[^A-Z]*A[^A-Z]*M[^A-Z]*X)/${smul}$1${rmul}/g;
  printf "%s %d\n", $_, count $str;
}

while (<>) {
  chomp;
  push @rows, $_;
  hilite $_;
  $total += count $_;

  my $len = length $_;
  for (my $x = 0; $x < $len; ++$x) {
    $columns[$x] .= substr $_, $x, 1;
    $left_diag[$x+$y] .= substr $_, $x, 1;
    $right_diag[$len-$x-1+$y] .= substr $_, $x, 1;
  }

  ++$y;
}

#for (my $i = 0; $i <= $#left_diag; ++$i) {
#  my $row = $i > $#rows ? '' : $rows[$i];
#  my $column = $i > $#columns ? '' : $columns[$i];
#  my $left_diag = "." x (19-$i) . $left_diag[$i];
#  printf "%10.10s %10.10s %-20.20s %-20.20s\n", $row, $column, $left_diag, $right_diag[$i];
#}

print "\n";
foreach my $column (@columns) { hilite $column; $total += count $column }
print "\n";
foreach my $diag (@left_diag) { hilite $diag; $total += count $diag }
print "\n";
foreach my $diag (@right_diag) { hilite $diag; $total += count $diag }

print "$total\n";

__END__

     Left
 y | 0 1 2 3 4 5 <- $x
---+------------
 0 | 0 1 2 3 4 5
 1 | 1 2 3 4 5 6 <- $left_diag[$x+$y] .= substr $_, $x, 1;
 2 | 2 3 4 5 6 7
 3 | 3 4 5 6 7 8
     
     Right
 0 | 5 4 3 2 1 0
 1 | 6 5 4 3 2 1 <- $right_diag[$len-$x-1+$y] .= substr $_, $x, 1;
 2 | 7 6 5 4 3 2
 3 | 8 7 6 5 4 3
