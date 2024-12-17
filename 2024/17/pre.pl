#!/usr/bin/perl
require 5;
use warnings;
use strict;

print "(";
while (<>) {
  chomp;
  next if /^$/;
  my($k, $v) = /^(.*?): (.*)/ or die "$_??";
  $k =~ s/\s+/-/g;
  if ($v =~ /,/) {
    $v =~ s/,/ /g;
    $v = "($v)";
  }
  print "\n  ($k . $v)";
}
print ")\n";
