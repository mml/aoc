#!/usr/bin/perl
require 5;
use warnings;
use strict;

while (<>) {
  s/#/##/g;
  s/O/[]/g;
  s/\./../g;
  s/\@/\@./g;
  print;
}
