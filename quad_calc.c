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

// s1x s2x mm delta sigma k
// (w-double-even-odd (/ 3 10) (/ 1 10) 40 (/ 7 10) (/ 4 10) (/ 2))

int main()
{
  enum{M=30};
  __float128 binom[M],s1x=.3,s2x=.1,delta=.7,sigma=.4,k=.5,sum=.0q;
  calc_binomials(M,binom);
  char y[1000];
  int i;
  for(i=0;i<M;i++){
    int m=i+1;
    __float128 a2=sigma*sigma*(2*m*delta*delta+4*sigma*sigma)/(m*delta*delta+4*sigma*sigma),
      b2=2/(m*delta*delta)+1/(sigma*sigma),
      c=k*k*sigma*sigma*m*delta*delta/(m*delta*delta+4*sigma*sigma),
      d=2*k*k*sigma*sigma*sigma*sigma/(m*delta*delta+4*sigma*sigma);
    sum += binom[i]*powq(-1.0q,m-1)*sqrtq(a2/(m*b2))*expq(-c*(s1x*s1x+s2x*s2x)-d*powq(s1x-s2x,2));
  }
    quadmath_snprintf(y, 1000, "%Qf", sum);
    printf("%s\n",y);
  return 0;
}
