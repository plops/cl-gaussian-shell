#include <quadmath.h>
#include <stdio.h>
#include <stdlib.h>

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

int compar(const void*a,const void*b)
{
  __float128 p=*((__float128*)a),q=*((__float128*)b), pp=fabsq(p),qq=fabsq(q);
  if(pp<qq)
    return -1;
  else if(pp==qq)
    return 0;
  else
    return 1;
}

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
    res[i]= binom[i]*sqrtq(a2/(m*b2))*expq(-c*(s1x*s1x+s2x*s2x)-d*powq(s1x-s2x,2));
    if(m%2==0)
      res[i]*=-1.0q;
  }
  qsort((void*)res,M,sizeof(__float128),compar);
  __float128 err=res[M-1]*FLT128_EPSILON;
  char str[1000];
  quadmath_snprintf(str, 1000, "%12.8Qf",  log10q(err));
  fprintf(stderr,"log10q(err[%12.8f]) = %s\n",(double)s1x,str);
  __float128 S=res[0], C=0.0q; // Kahan summation into S
  for(i=1;i<M;i++){
    __float128 Y=res[i]-C, 
      T=S+Y; // Alas, S is big, Y small, so low-order digits of Y are lost.
    C=(T-S)-Y; // T-S recovers the high order part of Y, subtracting Y gives the low order part
  
    { //if(C!=0.0q && C!=-0.0q){
      quadmath_snprintf(str, 1000, "%%12.8Qf", res[i]);
      fprintf(stderr,"val = %s\n",str);
    }
  
    S=T;
  }
  fprintf(stderr,"\n");
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
    quadmath_snprintf(y, 1000, "%12.8Qf", w(M,binom,.1*i,s2x,delta,sigma,k));
    printf("%12.8f %s\n",i*.1,y);
  }
  return 0;
}
