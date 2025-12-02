#!/usr/bin/perl

require 5;
use warnings;
use strict;

$/ = ',';
my $n = 0;

while (<>) {
  chomp;
  my($lo, $hi) = split /-/, $_;
  for (my $id = $lo; $id <= $hi; ++$id) {
    if ($id =~ /^(\d+)\1$/) {
      #print "<$id>\n";
      $n += $id;
    }
  }
}

print "$n\n";
