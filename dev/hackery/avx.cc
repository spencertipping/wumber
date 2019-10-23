#include <cmath>
#include <immintrin.h>
#include <stdio.h>


#define max(a, b) ((a) > (b) ? (a) : (b))

struct sphere
{
  double  const radius;
  __m256d const center;
};

static struct sphere our_union[3] = {
  { .radius = 5, .center = _mm256_set_pd(0, 1, 1, 1) },
  { .radius = 4, .center = _mm256_set_pd(0, 2, 2, 2) },
  { .radius = 3, .center = _mm256_set_pd(0, 3, 3, 3) }
};


static inline double avx_eval_sphere(sphere const &s, __m256d const &v)
{
  __m256d diff    = _mm256_sub_pd(s.center, v);
  __m256d squared = _mm256_mul_pd(diff, diff);
  return s.radius - sqrt(((double*)&squared)[1] +
                         ((double*)&squared)[2] +
                         ((double*)&squared)[3]);
}


static inline double avx_eval_union(__m256d const &v)
{
  double const s1 = avx_eval_sphere(our_union[0], v);
  double const s2 = avx_eval_sphere(our_union[1], v);
  double const s3 = avx_eval_sphere(our_union[2], v);

  return max(s1, max(s2, s3));
}


struct solution
{
  __m256d boundary;
  __m256d gradient;
};


#define EPSILON 1e-8

static solution solve(__m256d v)
{
  double d;
  __m256d g;

  while (1)
  {
    d = avx_eval_union(v);

    __m256d epx = v; ((double*)&epx)[1] += EPSILON;
    double const gx = (avx_eval_union(epx) - d) / EPSILON;

    __m256d epy = v; ((double*)&epy)[2] += EPSILON;
    double const gy = (avx_eval_union(epy) - d) / EPSILON;

    __m256d epz = v; ((double*)&epz)[3] += EPSILON;
    double const gz = (avx_eval_union(epz) - d) / EPSILON;

    g = _mm256_set_pd(0, gx, gy, gz);

    if (fabs(d) <= EPSILON)
      return { .boundary = v, .gradient = g };

    // Otherwise: adjust our vector.
    double  ad   = fabs(d) / 2;
    __m256d absd = _mm256_set_pd(0, ad, ad, ad);
    v = _mm256_sub_pd(v, _mm256_mul_pd(g, absd));

    if (0)
      printf("%f:  %f %f %f   %f %f %f\n",
             d,
             ((double*)&v)[1], ((double*)&v)[2], ((double*)&v)[3],
             ((double*)&g)[1], ((double*)&g)[2], ((double*)&g)[3]);
  }
}


int main()
{
  // Run 1M solutions of the union on the vector <0.1, 0.1, 0.1>.
  for (long i = 0; i < 1000000; ++i)
  {
    __m256d v = _mm256_set_pd(0, 0.1, 0.1, 0.1);
    solution s = solve(v);
  }

  return 0;
}
