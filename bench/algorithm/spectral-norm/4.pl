# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# Contributed by Andrew Rodland
# modified by R. Jelinek
# multicore by Mykola Zubach

use strict;
use threads;

my $cpus = num_cpus();

my $n = shift || 500;
my @v = multiplyAtAv(
    multiplyAtAv(
        multiplyAtAv((1) x $n)
    )
);

my @u = multiplyAtAv(@v);

my ($vBv, $vv);
my $i = 0;
for my $v (@v) {
    $vBv += $u[$i++] * $v;
    $vv += $v ** 2;
}

printf "%0.9f\n", sqrt($vBv / $vv);

sub multiplyAtAv {
    return multiplyAtv(multiplyAv(@_));
}

sub eval_A {
    use integer;
    my $div = (($_[0] + $_[1]) * ($_[0] + $_[1] + 1) >> 1) + $_[0] + 1;
    no integer;
    1 / $div;
}

sub multiplyAv {
    my($begin, $end, @threads);
    my $chunk = int($#_ / $cpus) + 1;
    
    for($begin = 0; $begin < $#_; $begin = $end + 1) {
        $end = $begin + $chunk;
        $end = $#_ if $end > $#_;
        push @threads, threads->create( sub {
            my $begin = shift;
            my $end = shift;
            return map {
                my ($i, $sum) = ($_);
                $sum += eval_A($i, $_) * $_[$_] for (0 .. $#_);
                $sum;
            } ($begin .. $end);
       }, $begin, $end, @_);
    }
    return map $_->join, @threads;
}

sub multiplyAtv {
    my($begin, $end, @threads);
    my $chunk = int($#_ / $cpus) + 1;
    
    for($begin = 0; $begin < $#_; $begin = $end + 1) {
        $end = $begin + $chunk;
        $end = $#_ if $end > $#_;
        push @threads, threads->create( sub {
            my $begin = shift;
            my $end = shift;
            return map {
                my ($i, $sum) = ($_);
                $sum += eval_A($_, $i) * $_[$_] for (0 .. $#_);
                $sum;
            } ($begin .. $end);
       }, $begin, $end, @_);
    }
    return map $_->join, @threads;
}

sub num_cpus {
    open my $fh, '</proc/cpuinfo' or return 4;
    my $cpus;
    while (<$fh>) {
        $cpus ++ if /^processor\s+:/;
    }
    return $cpus;
}