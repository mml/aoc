#!/usr/bin/perl
require 5;
use warnings;
use strict;

my $in = '';
my $dest;
my @n = (-1, -1, -1, -1, -1);
my @keys;
my @locks;

sub save_n {
  push @$dest, [@n];
  undef $dest;
  @n = (-1, -1, -1, -1, -1);
}

LINE: while (<<>>) {
  if (/^$/) {
    &save_n;
    next LINE;
  }
  unless (defined $dest) {
    if (/^#/) {
      $dest = \@locks;
    } elsif (/^\./) {
      $dest = \@keys;
    }
  }
  my $i = 0;
  while (s/^(.)//) {
    if ($1 eq '#') {
      ++$n[$i];
      die "overflow \$n[$i] $n[$i]" if $n[$i] > 5;
    }
    ++$i;
  }
}

if (defined $dest) {
  &save_n;
}

my $sum = 0;
foreach my $key (@keys) {
  LOCK: foreach my $lock (@locks) {
    for (0..4) {
      next LOCK if $lock->[$_] + $key->[$_] > 5;
    }
    ++$sum;
  }
}
print "sum = $sum\n";
