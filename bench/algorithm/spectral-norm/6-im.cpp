// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// Original C contributed by Sebastien Loisel
// Conversion to C++ by Jon Harrop
// OpenMP parallelize by The Anh Tran
// Add SSE by The Anh Tran
// Additional SSE optimization by Krzysztof Jakubowski

// g++ -pipe -O3 -march=native -fopenmp -mfpmath=sse -msse2 \
//     ./spec.c++ -o ./spec.run

#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <emmintrin.h>
#include <omp.h>
#include <sched.h>

template <bool modei> int Index(int i, int j) {
  return (((i + j) * (i + j + 1)) >> 1) + (modei ? i : j) + 1;
}

template <bool modei>
void EvalPart(double *__restrict__ src, double *__restrict__ dst, int begin,
              int end, int length) {
  int i = begin;

  for (; i + 1 < end; i += 2) {
    __m128d sum = _mm_set_pd(src[0] / double(Index<modei>(i + 1, 0)),
                             src[0] / double(Index<modei>(i + 0, 0)));

    __m128d ti = modei ? _mm_set_pd(i + 1, i + 0) : _mm_set_pd(i + 2, i + 1);
    __m128d last = _mm_set_pd(Index<modei>(i + 1, 0), Index<modei>(i + 0, 0));

    for (int j = 1; j < length; j++) {
      __m128d idx = last + ti + _mm_set1_pd(j);
      last = idx;
      sum = sum + _mm_set1_pd(src[j]) / idx;
    }

    _mm_storeu_pd(dst + i + 0, sum);
  }
  for (; i < end; i++) {
    double sum = 0;
    for (int j = 0; j < length; j++)
      sum += src[j] / double(Index<modei>(i, j));
    dst[i] = sum;
  }
}

void EvalATimesU(double *src, double *dst, int begin, int end, int N) {
  EvalPart<1>(src, dst, begin, end, N);
}

void EvalAtTimesU(double *src, double *dst, int begin, int end, int N) {
  EvalPart<0>(src, dst, begin, end, N);
}

void EvalAtATimesU(double *src, double *dst, double *tmp, int begin, int end,
                   int N) {
  EvalATimesU(src, tmp, begin, end, N);
#pragma omp barrier
  EvalAtTimesU(tmp, dst, begin, end, N);
#pragma omp barrier
}

int GetThreadCount() {
  cpu_set_t cs;
  CPU_ZERO(&cs);
  sched_getaffinity(0, sizeof(cs), &cs);

  int count = 0;
  for (int i = 0; i < CPU_SETSIZE; ++i)
    if (CPU_ISSET(i, &cs))
      ++count;

  return count;
}

double spectral_game(int N) {
  __attribute__((aligned(16))) double u[N];
  __attribute__((aligned(16))) double v[N], tmp[N];

  double vBv = 0.0;
  double vv = 0.0;

#pragma omp parallel default(shared) num_threads(GetThreadCount())
  {
    // this block will be executed by NUM_THREADS
    // variable declared in this block is private for each thread
    int threadid = omp_get_thread_num();
    int threadcount = omp_get_num_threads();
    int chunk = N / threadcount;

    // calculate each thread's working range [r1 .. r2) => static schedule
    int begin = threadid * chunk;
    int end = (threadid < (threadcount - 1)) ? (begin + chunk) : N;

    for (int i = begin; i < end; i++)
      u[i] = 1.0;
#pragma omp barrier

    for (int ite = 0; ite < 10; ++ite) {
      EvalAtATimesU(u, v, tmp, begin, end, N);
      EvalAtATimesU(v, u, tmp, begin, end, N);
    }

    double sumvb = 0.0, sumvv = 0.0;
    for (int i = begin; i < end; i++) {
      sumvv += v[i] * v[i];
      sumvb += u[i] * v[i];
    }

#pragma omp critical
    {
      vBv += sumvb;
      vv += sumvv;
    }
  }

  return sqrt(vBv / vv);
}

int main(int argc, char *argv[]) {
  int N = ((argc >= 2) ? atoi(argv[1]) : 2000);
  printf("%.9f\n", spectral_game(N));
  return 0;
}
