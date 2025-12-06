#!/usr/bin/perl

require 5;
use warnings;
use strict;

use feature 'signatures';

my @column;

sub product ($i) {
  my $rv = 1;
  for (@{$column[$i]}) { $rv *= $_ }
  return $rv;
}

sub sum ($i) {
  my $rv = 0;
  for (@{$column[$i]}) { $rv += $_ }
  return $rv;
}

my $tot = 0;

while (<>) {
  chomp;
  my @tokens = split ' ', $_;
  if (/\d/) {
    for (my $i = 0; $i <= $#tokens; ++$i) {
      push @{$column[$i]}, $tokens[$i];
    }
  } else {
    for (my $i = 0; $i <= $#tokens; ++$i) {
      if ($tokens[$i] eq '*') {
        $tot += product($i);
      } elsif ($tokens[$i] eq '+') {
        $tot += sum($i);
      } else {
        die;
      }
    }
  }
}
print "$tot\n";
