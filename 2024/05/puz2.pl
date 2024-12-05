#!/usr/bin/perl
require 5;
use warnings;
use strict;

my %before;

while (<>) {
  last if /^\s*$/;
  die unless /^(\d+)\|(\d+)$/;
  push @{$before{$1}}, $2;
}

my $total = 0;
UPDATE: while (<>) {
  my $correct = 1;
  #print "==$_";
  chomp;
  my @pages = split /,/;
  my %index;
  for (my $i = 0; $i <= $#pages; ++$i) {
    $index{$pages[$i]} = $i;
  }
  CHECK:
  print "Check...";
  for (my $i = 0; $i <= $#pages; ++$i) {
    foreach my $page (@{$before{$pages[$i]}}) {
      if (exists $index{$page}) {
        if ($index{$page} > $i) {
          #print "index{$page} = $index{$page} > $i\n";
        } else {
          print "index{$page} = $index{$page} <= $i\n";
          my $swap = $pages[$i];
          splice @pages, $i, 1, $page;
          splice @pages, $index{$page}, 1, $swap;
          $index{$swap} = $index{$page};
          $index{$page} = $i;
          $correct = 0;
          goto CHECK;
        }
      }
    }
  }
  next if $correct;
  print "$_\n";
  $total += $pages[$#pages/2];
}
print "$total\n";
