#include <quadmath.h>
#include <stdio.h>

__uint128_t factorial(__uint128_t n, __uint128_t acc)
{
  if(n<=1)
    return acc;
  else
    return factorial(n-1,acc*n);
}

__float128 nchoosek(unsigned int n, unsigned int k)
{
  return factorial(n,1)/(1.0q*factorial(k,1)*factorial(n-k,1));
}

int main()
{
  __float128 a=2.0q;
  char y[1000];
  quadmath_snprintf(y, 1000, "%Qf", nchoosek(50,2));
  printf("%s\n",y);
  return 0;
}
