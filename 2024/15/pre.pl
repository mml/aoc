#!/usr/bin/perl
require 5;
use warnings;
use strict;

print "(\n";
while (<STDIN>) {
  last if /^$/;
  chomp;
  print " (";
  print join ' ', map { "#\\$_" } split //;
  print ")\n";
}
print ")\n";

print "(\n";
while (<STDIN>) {
  chomp;
  while (s/^(.)//) {
    print '  ';
    if ('<' eq $1) {
      print 'left';
    } elsif ('^' eq $1) {
      print 'up';
    } elsif ('>' eq $1) {
      print 'right';
    } elsif ('v' eq $1) {
      print 'down';
    } else {
      die;
    }
    print "\n";
  }
}
print ")\n";
