#include <math.h>
#include <Rmath.h>
#include <R.h>
#include <Rcpp.h>
using namespace Rcpp;
double bs(int type, double S, double K, double sigma, double t, double r){
  double d1,d2,val;
  d1 = (log(S/K) + (r+pow(sigma,2)/2)*t) / (sigma*sqrt(t));
  d2 = (log(S/K) + (r-pow(sigma,2)/2)*t) / (sigma*sqrt(t));
  if(type==0) val = R::pnorm(d1,0.0,1.0,TRUE,FALSE)*S
  - R::pnorm(d2,0.0,1.0,TRUE,FALSE)*K*exp(-r*t);
  else if (type==1) val = R::pnorm(-d2,0.0,1.0,TRUE,FALSE)*K*exp(-r*t)
  - R::pnorm(-d1,0.0,1.0,TRUE,FALSE)*S;
  return val;
}
// [[Rcpp::export]]
double secant(int type, double V, double S, double K,
      double sigma0, double sigma1, double t, double r){
  if( fabs(sigma0-sigma1) < .001 ) return(sigma0);
  else{
    double newSigma = sigma0 - (bs(type,S,K,sigma0,t,r)-V)*
        (sigma0-sigma1)/
        (bs(type,S,K,sigma0,t,r) - bs(type,S,K,sigma1,t,r));
    return(secant(type,V,S,K,newSigma,sigma0,t,r));
  }
}
