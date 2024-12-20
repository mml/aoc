#!/usr/bin/perl
my $i = 0;
my $n = 0;
while (<<>>) {
  chomp;
  while (length) {
    ++$i;
    if (s/^\(//) { ++$n }
    elsif (s/^\)//) { --$n }

    if ($n == -1) {
      print "$i\n";
      exit;
    }
  }
}
die;
