#!/usr/bin/perl
require 5;
use warnings FATAL => qw(all);
use strict;

use feature 'signatures';

die unless $#ARGV == 1;
my $w = shift;
my $h = shift;
my(@px, @py, @vx, @vy);

while (<STDIN>) {
  die unless /^p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)$/;
  push @px, $1;
  push @py, $2;
  push @vx, $3;
  push @vy, $4;
}

my @count;
for (my $y = 0; $y < $h; $y++) {
  my $row = [];
  for (my $x = 0; $x < $w; $x++) {
    push @$row, 0;
  }
  push @count, $row;
}

for (my $i = 0; $i <= $#px; $i++) {
  my $px = ($px[$i] + 100*$vx[$i]) % $w;
  my $py = ($py[$i] + 100*$vy[$i]) % $h;
  $count[$py][$px]++;
}

for (my $y = 0; $y < $h; $y++) {
  for (my $x = 0; $x < $w; $x++) {
    if ($count[$y][$x]) {
      print $count[$y][$x];
    } else {
      print '.';
    }
  }
  print "\n";
}
#exit;

my $qw = int($w/2);
my $qh = int($h/2);

sub qsum ($q) {
  my($xstart, $xstop, $ystart, $ystop);
  if ($q == 1 or $q == 4) {
    $xstart = $qw+1;
    $xstop = $w;
  } else {
    $xstart = 0;
    $xstop = $qw;
  }
  if ($q == 3 or $q == 4) {
    $ystart = $qh + 1;
    $ystop = $h;
  } else {
    $ystart = 0;
    $ystop = $qh;
  }

  my $sum = 0;
  for (my $x = $xstart; $x < $xstop; $x++) {
    for (my $y = $ystart; $y < $ystop; $y++) {
      $sum += $count[$y][$x];
    }
  }
  print "Q$q = x=[$xstart,$xstop) y=[$ystart,$ystop) $sum\n";
  return $sum;
}

my $safety_factor = 1;
for (1..4) {
  $safety_factor *= qsum($_);
}
print "$safety_factor\n";
