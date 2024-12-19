#!/usr/bin/perl
require 5;
use warnings;
use strict;

my $towels = <>;
chomp $towels;
my @towels = split /, /, $towels;

$_ = <>; # skip blank line
my $towel_pat = '(' . (join '|', @towels) . ')';

my @patterns;
my $n = 0;
while (<>) {
  if (/^${towel_pat}*$/) {
    print "+ $_";
    ++$n;
  } else {
    print "- $_";
  }
}

print "n = $n\n";
