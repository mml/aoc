#!/usr/bin/perl
require 5;
use warnings;
use strict;

use feature 'signatures';

while (<<>>) {
  last if /^$/;
  chomp;
  if (m/^(.*):\s*(.*)$/) {
    print "(define ($1) $2)\n";
  } else {
    die;
  }
}

my %f = (
  XOR => 'logxor',
  OR => 'logor',
  AND => 'logand',
);
my $maxz = 0;
while (<<>>) {
  if (m/([a-z][a-z0-9]+) ([A-Z]+) ([a-z][a-z0-9]+) -> (([a-z])([a-z0-9]+))/) {
    print "(define ($4) ($f{$2} ($1) ($3)))\n";
    if ($5 eq 'z') {
      $maxz = $6 if $6 > $maxz;
    }
  } else {
    die;
  }
}

print "(define (z) (+\n";
for (my $i = 0; $i <= $maxz; ++$i) {
  printf "  (ash (z%02d) %d)\n", $i, $i;
}
print "))\n";

print "(display (z)) (newline)\n";
