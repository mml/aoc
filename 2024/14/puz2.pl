#!/usr/bin/perl
require 5;
use warnings FATAL => qw(all);
use strict;

use feature 'signatures';

# Presented with all warts and blind alleys intact.  Part 1 was so easy, I just
# did it in Perl.  I had my doubts about doing Part 2 that way, but carried on
# regardless.  This spaghetti monster was the result!

$| = 1;

die unless $#ARGV == 1;
my $w = shift;
my $h = shift;
my(@Px, @Py, @Vx, @Vy);
our($Px, $Py, $Vx, $Vy);

while (<STDIN>) {
  die unless /^p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)$/;
  push @Px, $1;
  push @Py, $2;
  push @Vx, $3;
  push @Vy, $4;
}

sub for_robots :prototype(&@) {
  my $f = shift;
  my @robots = $#_ >= 0 ? @_ : (0..$#Px);

  foreach my $i (@robots) {
    local $Px = $Px[$i];
    local $Py = $Py[$i];
    local $Vx = $Vx[$i];
    local $Vy = $Vy[$i];
    local $_ = $i;
    &$f;
  }
}
sub xt ($t) { ($Px + $t * $Vx) % $w }
sub yt ($t) { ($Py + $t * $Vy) % $h }

sub divmod ($dend, $isor) {
  my $quo = int($dend / $isor);
  my $rem = $dend % $isor;
  return $quo, $rem;
}

my $Ytarget = 50;

sub span_hunt ($t101, @robots) {
  my $span_min = 1e6;
  for (my $t = $t101; $t < 10404; $t += 101) {
    my $ymax = 0;
    my $ymin = 1e6;
    for_robots {
      my($x, $y) = (xt($t), yt($t));
      #printf "i=%3d t=%3d (%d,%d)\n", $_, $t, xt($t), yt($t);
      $ymin = $y if $y < $ymin;
      $ymax = $y if $y > $ymax;
    } @robots;
    my $span = $ymax - $ymin;
    if ($span < $span_min) {
      $span_min = $span;
      printf "span=$span @ t=$t\n";
    }
  }
}

span_hunt(100,  54, 205, 336, 436);
span_hunt(98,   117, 153, 230, 361);
span_hunt(0,    64, 70, 189, 405);
#exit;

my $nope = 0;
for_robots {
  print "$nope\r";
  my $dividend = ($Ytarget-$Px);
  my($quo, $rem) = divmod($dividend, $Vx);
  if (0 == $rem) {
    printf "$_ (%d-%d) / %d", $Ytarget, $Px, $Vx;
    printf "\t= %d / %d", $dividend, $Vx;
    printf "\t= %d\t\t(mod 101)\n", $quo % 101;
  } else {
    $nope++;
  }
};
print "nope=$nope\n";
#exit;


my @count;
for (my $y = 0; $y < $h; $y++) {
  my $row = [];
  for (my $x = 0; $x < $w; $x++) {
    push @$row, 0;
  }
  push @count, $row;
}

sub width_score ($y) {
  my $score = 0;
  for (my $x = 0; $x < $w; ++$x) {
    if (my $n = $count[$y][$x]) {
      my $dx = 50-$x;
      $score += $n * $dx * $dx;
    }
  }
  return $score;
}

sub top_width_score {
  my $score = 0;
  for (my $y = 0; $y < 37; ++$y) {
    $score += width_score($y);
  }
  return $score;
}

sub same_x_score {
  my $score = 0;
  for (my $x = 0; $x < $w; ++$x) {
    my $n = 0;
    for (my $y = 0; $y < $h; ++$y) {
      $n += $count[$y][$x];
    }
    $score += $n*$n;
  }
  return -$score;
}

sub same_y_score {
  my $score = 0;
  for (my $y = 0; $y < $h; ++$y) {
    my $n = 0;
    for (my $x = 0; $x < $w; ++$x) {
      $n += $count[$y][$x];
    }
    $score += $n*$n;
  }
  return -$score;
}

sub diagonal_score {
  # points for each tile where a robot is present
  # and also a robot is present at
  # (x-1,y+1) or (x+1,y+1)
  my $score = 0;
  for (my $y = 0; $y < $h-1; ++$y) {
    for (my $x = 1; $x < $w-1; ++$x) {
      next unless $count[$y][$x];
      my $y2 = $y+1;
      foreach my $x2 ($x-1, $x+1) {
        next if $x2<0 or $x2 >= $w;
        $score++ if $count[$y2][$x2];
      }
    }
  }
  return -$score
}

sub symmetry {
  my $score = 0;
  for (my $y = 0; $y < $h; $y++) {
    for (my $x = 0; $x < $w; $x++) {
      $score += abs($count[$y][$x] - $count[$y][$w-$x-1]);
    }
  }
  return $score;
}

sub top_bottom_delta {
  my $score = 0;
  for (my $y = $h-50; $y < $h; $y++) {
    for (my $x = 0; $x < $w; $x++) {
      $score += $count[$y][$x];
    }
  }
  for (my $y = 0; $y < 50; $y++) {
    for (my $x = 0; $x < $w; $x++) {
      $score -= $count[$y][$x];
    }
  }
  return $score;
}

sub count_at_time ($t) {
  for (my $y = 0; $y < $h; $y++) {
    for (my $x = 0; $x < $w; $x++) {
      $count[$y][$x] = 0;
    }
  }
  for_robots {
    $count[yt($t)][xt($t)]++;
  };
}

sub show_it ($t = undef) {
  if (defined $t) {
    count_at_time($t);
  }
  for (my $y = 0; $y < $h; $y++) {
    for (my $x = 0; $x < $w; $x++) {
      if ($count[$y][$x]) {
        print $count[$y][$x];
      } else {
        print '.';
      }
    }
    print "\n";
  }
}

sub for_all_time :prototype(&) {
  my $f = shift;
  for (my $t = 0; $t <= $w*$h; ++$t) {
    local $_ = $t;
    &$f;
  }
}

sub find_minima ($f, $n = 1) {
  my @scores;
  my @ts;
  for_all_time {
    my $t = $_;
    print "t=$t\r";
    for_robots {
      unless ($t == 0) {
        $count[$Py][$Px]--;
      }
      $Px[$_] += $Vx;
      $Px[$_] %= $w;
      $Py[$_] += $Vy;
      $Py[$_] %= $h;
      $count[$Py[$_]][$Px[$_]]++;
    };
    push @scores, &$f;
  };
  my @sorted = sort { $scores[$a] <=> $scores[$b] } (0..$#scores);
  foreach my $t (reverse (@sorted[0..$n-1])) {
    show_it($t);
    printf "t=%d score=$scores[$t]\n", $t+1; # Not sure how off-by-1 in this direction
  }
}

#find_minima(\&top_width_score, 1);
#find_minima(\&same_y_score, 3);
#find_minima(\&same_x_score, 3);
find_minima(\&diagonal_score);
exit;

my $min_symmetry = 999_999_999;
my $max_delta = -999_999_999;
my $min_diag = 1e6;
for_all_time {
  my $t = $_;
  print "$t\r";
  for_robots {
    unless ($t == 0) {
      $count[$Py][$Px]--;
    }
    $Px[$_] += $Vx;
    $Px[$_] %= $w;
    $Py[$_] += $Vy;
    $Py[$_] %= $h;
    $count[$Py[$_]][$Px[$_]]++;
  };
  my $score = symmetry();
  if ($score < $min_symmetry) {
    $min_symmetry = $score;
    #show_it();
    #print "score=$score iters=$iters\n";
  }
  my $delta = top_bottom_delta();
  if ($delta > $max_delta) {
    $max_delta = $delta;
    show_it();
    print "delta=$delta t=$t\n";
  }
};

my $qw = int($w/2);
my $qh = int($h/2);

sub qsum ($q) {
  my($xstart, $xstop, $ystart, $ystop);
  if ($q == 1 or $q == 4) {
    $xstart = $qw+1;
    $xstop = $w;
  } else {
    $xstart = 0;
    $xstop = $qw;
  }
  if ($q == 3 or $q == 4) {
    $ystart = $qh + 1;
    $ystop = $h;
  } else {
    $ystart = 0;
    $ystop = $qh;
  }

  my $sum = 0;
  for (my $x = $xstart; $x < $xstop; $x++) {
    for (my $y = $ystart; $y < $ystop; $y++) {
      $sum += $count[$y][$x];
    }
  }
  print "Q$q = x=[$xstart,$xstop) y=[$ystart,$ystop) $sum\n";
  return $sum;
}

my $safety_factor = 1;
for (1..4) {
  $safety_factor *= qsum($_);
}
print "$safety_factor\n";
