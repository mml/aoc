#!/usr/bin/perl
while (<<>>) {
  ++$n while s/\(//;
  --$n while s/\)//;
}
print "$n\n";
