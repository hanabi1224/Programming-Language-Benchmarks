# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# contributed by Christoph Bauer
# converted into Perl by Márton Papp
# fixed and cleaned up by Danny Sauer
# optimized by Jesse Millikan
# optimized by Reini Urban

use constant PI            => 3.141592653589793;
use constant SOLAR_MASS    => (4 * PI * PI);
use constant DAYS_PER_YEAR => 365.24;

sub energy;
sub advance($);
sub offset_momentum;

my (@xs, @ys, @zs, @vxs, @vys, @vzs, @mass, $last);
my ($energy, $offset_momentum, $advance);
BEGIN {
# Global lexicals for arrays.
# Almost every iteration is a range, so I keep the last index rather than a count.

# @ns = ( sun, jupiter, saturn, uranus, neptune )
@xs = (0, 4.84143144246472090e+00, 8.34336671824457987e+00, 1.28943695621391310e+01, 1.53796971148509165e+01);
@ys = (0, -1.16032004402742839e+00, 4.12479856412430479e+00, -1.51111514016986312e+01, -2.59193146099879641e+01);
@zs = (0, -1.03622044471123109e-01, -4.03523417114321381e-01, -2.23307578892655734e-01, 1.79258772950371181e-01);
@vxs = map {$_ * DAYS_PER_YEAR}
  (0, 1.66007664274403694e-03, -2.76742510726862411e-03, 2.96460137564761618e-03, 2.68067772490389322e-03);
@vys = map {$_ * DAYS_PER_YEAR}
  (0, 7.69901118419740425e-03, 4.99852801234917238e-03, 2.37847173959480950e-03, 1.62824170038242295e-03);
@vzs = map {$_ * DAYS_PER_YEAR}
  (0, -6.90460016972063023e-05, 2.30417297573763929e-05, -2.96589568540237556e-05, -9.51592254519715870e-05);
@mass = map {$_ * SOLAR_MASS}
  (1, 9.54791938424326609e-04, 2.85885980666130812e-04, 4.36624404335156298e-05, 5.15138902046611451e-05);
$last = $#xs;

# Optimize array accesses: $a[const] are optimized to AELEMFAST, $a[$lexical] not.
# So unroll the loops in macro-like fashion (2x times faster). We do it in a BEGIN block,
# so perlcc can also benefit (again 2x faster).
sub qv {
  my $s = shift;
  my $env = shift;
  # expand our local loop vars
  $s =~ s/(\$\w+?)\b/exists($env->{$1})?$env->{$1}:$1/sge;
  $s
}

$energy = '
sub energy
{
  my $e = 0.0;
  my ($dx, $dy, $dz, $distance);';
  for my $i (0 .. $last) {
    my $env = {'$i'=>$i,'$last'=>$last};
    $energy .= qv('
    # outer-loop $i..4
    $e += 0.5 * $mass[$i] *
          ($vxs[$i] * $vxs[$i] + $vys[$i] * $vys[$i] + $vzs[$i] * $vzs[$i]);', $env);
    for (my $j = $i + 1; $j < $last + 1; $j++) {
      $env->{'$j'} = $j;
      $energy .= qv('
      # inner-loop $j..4
      $dx = $xs[$i] - $xs[$j];
      $dy = $ys[$i] - $ys[$j];
      $dz = $zs[$i] - $zs[$j];
      $distance = sqrt($dx * $dx + $dy * $dy + $dz * $dz);
      $e -= ($mass[$i] * $mass[$j]) / $distance;', $env);
    }
  }
  $energy .= '
  return $e;
}';
eval $energy; die if $@;

$advance = '
sub advance($)
{
  my $dt = $_[0];
  my ($mm, $mm2, $j, $dx, $dy, $dz, $distance, $mag);';
  for my $i (0..$last) {
    my $env = {'$i'=>$i};
    for (my $j = $i + 1; $j < $last + 1; $j++) {
      $env->{'$j'} = $j;
      $advance .= qv('
      # outer-loop $i..4
      # inner-loop $j..4
      $dx = $xs[$i] - $xs[$j];
      $dy = $ys[$i] - $ys[$j];
      $dz = $zs[$i] - $zs[$j];
      $distance = sqrt($dx * $dx + $dy * $dy + $dz * $dz);
      $mag = $dt / ($distance * $distance * $distance);
      $mm  = $mass[$i] * $mag;
      $mm2 = $mass[$j] * $mag;
      $vxs[$i] -= $dx * $mm2;
      $vxs[$j] += $dx * $mm;
      $vys[$i] -= $dy * $mm2;
      $vys[$j] += $dy * $mm;
      $vzs[$i] -= $dz * $mm2;
      $vzs[$j] += $dz * $mm;', $env);
    }
  }
  # We're done with planet $i at this point
  for my $i (0..$last) {
    my $env = {'$i'=>$i};
    $advance .= qv('
    $xs[$i] += $dt * $vxs[$i];
    $ys[$i] += $dt * $vys[$i];
    $zs[$i] += $dt * $vzs[$i];', $env);
  }
  $advance .= '
}';
eval $advance; die if $@;

$offset_momentum = ';
sub offset_momentum
{
  my $px = 0.0; 
  my $py = 0.0;
  my $pz = 0.0;
  my $mass;
';
for my $i (0 .. $last) {
  my $env = {'$i'=>$i};
  $offset_momentum .= qv('
    $mass = $mass[$i];
    $px += $vxs[$i] * $mass;
    $py += $vys[$i] * $mass;
    $pz += $vzs[$i] * $mass;', $env);
}
$offset_momentum .= '
  $vxs[0] = - $px / SOLAR_MASS;
  $vys[0] = - $py / SOLAR_MASS;
  $vzs[0] = - $pz / SOLAR_MASS;
}';
eval $offset_momentum; die if $@;

} #BEGIN

offset_momentum();
printf ("%.9f\n", energy());

my $n = $ARGV[0];
$n =~ s/[,_]//g; # allow 50_000_000 or 50,000,000

# This does not, in fact, consume N*4 bytes of memory
for (1 .. $n) {
  advance(0.01);
}

printf ("%.9f\n", energy());