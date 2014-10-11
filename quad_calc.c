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
  if(k==1)
    return n;
  else
    return factorial(n,1)/(1.0q*factorial(k,1)*factorial(n-k,1));
}

void
calc_binomials(int M,__float128*a)
{
  int m;
  __float128 Moverm=M;
  a[0]=Moverm;
  for(m=1;m<M;m++){
    Moverm *= (M-m)*1.0q/(m+1);
    a[m]=Moverm;
  }
}

int main()
{
  enum{M=12};
  __float128 a[M];
  calc_binomials(M,a);
  char y[1000];
  int i;
  for(i=0;i<M;i++){
    quadmath_snprintf(y, 1000, "%Qf", a[i]);
    printf("%s\n",y);
  }
  return 0;
}
