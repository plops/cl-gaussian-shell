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

__float128 w(int M,__float128*binom,__float128 s1x,__float128 s2x, __float128 delta, __float128 sigma, __float128 k)
{
  __float128 res[M];
  int i;
  for(i=0;i<M;i++){
    int m=i+1;
    __float128 d2=delta*delta,s2=sigma*sigma,
      div=1.0q/(m*d2+4.0q*s2),
      a2=s2*(2.0q*m*d2+4.0q*s2)*div,
      b2=2.0q/(m*d2)+1.0q/s2,
      k2=k*k,
      c=k2*s2*m*d2*div,
      d=2*k2*s2*s2*div;
    res[i]= binom[i]*powq(-1.0q,m-1)*sqrtq(a2/(m*b2))*expq(-c*(s1x*s1x+s2x*s2x)-d*powq(s1x-s2x,2));
  }
  __float128 S=res[0], C=0.0q; // Kahan summation into S
  for(i=1;i<M;i++){
    __float128 Y=res[i]-C,
      T=S+Y;
    C=(T-S)-Y;
    S=T;
  }
  return S;
}

int main()
{
  enum{M=50};
  __float128 binom[M],s1x=.3,s2x=.1,delta=.7,sigma=.4,k=.5,sum=.0q;
  calc_binomials(M,binom);
  char y[1000];
  int i;
  for(i=-250;i<=250;i++){
    quadmath_snprintf(y, 1000, "%Qf", w(M,binom,.1*i,s2x,delta,sigma,k));
    printf("%f %s\n",i*.1,y);
  }
  return 0;
}
