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

my @indices;
my @opers;
sub find_indices ($line) {
  for (my $i = 0; $i < length $line; ++$i) {
    my $char = substr $line, $i, 1;
    if ($char  ne ' ') {
      push @indices, $i;
      push @opers, $char;
    }
  }
}

my @lines;
while (<>) {
  chomp;
  if (/\d/) {
    push @lines, $_;
  } else {
    find_indices($_);
  }
}

sub column ($ii) {
  my @rv;
  my $i = $indices[$ii];
  my $ni = $ii < $#indices ? $indices[$ii+1] : undef;
  foreach my $line (@lines) {
    if (defined $ni) {
      push @rv, substr $line, $i, $ni-$i-1; # -1 for the blank
    } else {
      push @rv, substr $line, $i;
    }
  }
  return @rv;
}

my $tot = 0;

for (my $ii = 0; $ii <= $#indices; ++$ii) {
  my $oper = $opers[$ii];
  my $ans = $oper eq '+' ? 0 : 1;
  my @column = column($ii);
  # for (@column) { print "$_\n" }
  # print "$oper\n\n";
  NUMBER: for (my $j = -1;; $j--) {
    my $number = 0;
    ROW: foreach my $row (@column) {
      my $digit = substr $row, $j, 1;
      last NUMBER if '' eq $digit;
      next ROW if ' ' eq $digit;
      $number = $number * 10 + $digit;
    }
    eval "\$ans ${oper}= $number";
    #print "\$ans ${oper}= $number (= $ans)\n";
  }
  $tot += $ans;
}

print "$tot\n";
