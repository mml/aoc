#!/usr/bin/perl
require 5;
use warnings;
use strict;

use feature 'signatures';
$| = 1;

my $towels = <>;
chomp $towels;
my @towels = split /, /, $towels;
<>;

our @starts;
our @stops;

sub prefixes ($pos) {
  my @rv;
  for (my $i = 0; $i <= $#starts; ++$i) {
    if ($starts[$i] == $pos) {
      $rv[$stops[$i]] //= 0;
      ++$rv[$stops[$i]];
    }
  }
  return [@rv];
}

my $sum = 0;
while (<>) {
  chomp;
  print "$_ ";
  my $pattern = $_;
  my $len = length;
  @starts = ();
  @stops = ();
  my @which;
  TOWEL: for (my $i = 0; $i <= $#towels; ++$i) {
    #print "towel[$i]\n";
    my $towel = $towels[$i];
    my $len_towel = length $towel;
    for (my $j = 0; $j < $len; ) {
      my $pos = index $_, $towel, $j;
      next TOWEL if $pos < 0;
      push @starts, $pos;
      push @stops, $pos + $len_towel;
      push @which, $towel;
      #printf "$pattern/$towel [$pos,%d)\n", $pos+$len_towel;
      $j = $pos+1;
    }
  }
  my @ways = (1);
  for (my $pos = 0; $pos < $len; ++$pos) {
    next unless defined $ways[$pos];
    my $ways = prefixes($pos);
    foreach (my $j = 0; $j <= $#$ways; ++$j) {
      next unless defined $ways->[$j];
      $ways[$j] //= 0;
      $ways[$j] += $ways->[$j] * $ways[$pos];
    }
  }

  if ($#ways != $len) {
    print "0\n";
  } else {
    print "$ways[$#ways]\n";
    $sum += $ways[$#ways];
  }
}

print "$sum\n";
