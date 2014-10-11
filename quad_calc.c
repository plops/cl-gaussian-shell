#include <quadmath.h>
#include <stdio.h>
int main()
{
  __float128 a=2.0q;
  sqrtq(a);
  char y[1000];
  quadmath_snprintf(y, 1000, "%Qf", 1.0q);
  printf("%s\n",y);
  return 0;
}
