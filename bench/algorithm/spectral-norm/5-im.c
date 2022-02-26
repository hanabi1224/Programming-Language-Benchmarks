/* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * contributed by Ledrug
 * algorithm is a straight copy from Steve Decker et al's Fortran code
 * with GCC SSE2 intrinsics
 */

#include <emmintrin.h>
#include <malloc.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

inline double A(int i, int j) { return ((i + j) * (i + j + 1) / 2 + i + 1); }

double dot(double *v, double *u, int n) {
  int i;
  double sum = 0;
  for (i = 0; i < n; i++)
    sum += v[i] * u[i];
  return sum;
}

void mult_Av(double *v, double *out, const int n) {
  int i;

#pragma omp parallel for
  for (i = 0; i < n; i++) {
    __m128d sum = _mm_setzero_pd();

    int j;
    for (j = 0; j < n; j += 2) {
      __m128d b = _mm_set_pd(v[j], v[j + 1]);
      __m128d a = _mm_set_pd(A(i, j), A(i, j + 1));
      sum = _mm_add_pd(sum, _mm_div_pd(b, a));
    }
    out[i] = sum[0] + sum[1];
  }
}

void mult_Atv(double *v, double *out, const int n) {
  int i;

#pragma omp parallel for
  for (i = 0; i < n; i++) {
    __m128d sum = _mm_setzero_pd();

    int j;
    for (j = 0; j < n; j += 2) {
      __m128d b = _mm_set_pd(v[j], v[j + 1]);
      __m128d a = _mm_set_pd(A(j, i), A(j + 1, i));
      sum = _mm_add_pd(sum, _mm_div_pd(b, a));
    }
    out[i] = sum[0] + sum[1];
  }
}

double *tmp;
void mult_AtAv(double *v, double *out, const int n) {
  mult_Av(v, tmp, n);
  mult_Atv(tmp, out, n);
}

int main(int argc, char **argv) {
  int n = atoi(argv[1]);
  if (n <= 0)
    n = 2000;
  if (n & 1)
    n++; // make it multiple of two

  double *u, *v;
  u = memalign(16, n * sizeof(double));
  v = memalign(16, n * sizeof(double));
  tmp = memalign(16, n * sizeof(double));

  int i;
  for (i = 0; i < n; i++)
    u[i] = 1;
  for (i = 0; i < 10; i++) {
    mult_AtAv(u, v, n);
    mult_AtAv(v, u, n);
  }

  printf("%.9f\n", sqrt(dot(u, v, n) / dot(v, v, n)));

  return 0;
}