#!/usr/bin/perl
require 5;
use warnings;

my $sum = 0;
my $do = 1;

while (<>) {
  while (s{
      (
        mul\((?<a>\d{1,3}),(?<b>\d{1,3})\)
        |
        (?<on>do\(\))
        |
        (?<off>don't\(\))
      )
    }{}x) {
    #print "$1 * $2\n";
    #warn "@{[keys %+]}\n";
    if ($do and exists $+{a}) {
      $sum += $+{a}*$+{b};
    } elsif (exists $+{on}) {
      $do = 1;
    } elsif (exists $+{off}) {
      $do = 0;
    }
  }
}

print "$sum\n";
