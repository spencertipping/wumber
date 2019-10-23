#include <cmath>
#include <stdio.h>


#define max(a, b) ((a) > (b) ? (a) : (b))


struct sphere
{
  double const radius;
  double const center[3];
};

static struct sphere our_union[3] = {
  { .radius = 5, .center = { 1, 1, 1 } },
  { .radius = 4, .center = { 2, 2, 2 } },
  { .radius = 3, .center = { 3, 3, 3 } }
};


static inline double eval_sphere(sphere const &s,
                                 double const x,
                                 double const y,
                                 double const z)
{
  double const dx = s.center[0] - x;
  double const dy = s.center[1] - y;
  double const dz = s.center[2] - z;
  return s.radius - sqrt(dx*dx + dy*dy + dz*dz);
}


static inline double eval_union(double const x,
                                double const y,
                                double const z)
{
  double const s1 = eval_sphere(our_union[0], x, y, z);
  double const s2 = eval_sphere(our_union[1], x, y, z);
  double const s3 = eval_sphere(our_union[2], x, y, z);

  return max(s1, max(s2, s3));
}


struct solution
{
  double const boundary[3];
  double const gradient[3];
};


#define EPSILON 1e-8

static solution solve(double x, double y, double z)
{
  double d;

  while (1)
  {
    d = eval_union(x, y, z);

    double const gx = (eval_union(x + EPSILON, y, z) - d) / EPSILON;
    double const gy = (eval_union(x, y + EPSILON, z) - d) / EPSILON;
    double const gz = (eval_union(x, y, z + EPSILON) - d) / EPSILON;

    if (fabs(d) <= EPSILON)
      return { .boundary = { x, y, z }, .gradient = { gx, gy, gz } };

    // Otherwise: adjust our vector.
    double const ad = fabs(d) / 2;
    x -= gx * ad;
    y -= gy * ad;
    z -= gz * ad;
  }
}


int main()
{
  for (long i = 0; i < 1000000; ++i)
  {
    solution s = solve(0.1, 0.1, 0.1);
  }

  return 0;
}
