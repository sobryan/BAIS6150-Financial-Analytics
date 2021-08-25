#include <math.h>
#include <Rmath.h>
#include <R.h>
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]

double bs(double S, double K, double sigma, double t, double r){
double d1,d2,val;
d1 = (log(S/K) + (r+pow(sigma,2)/2)*t) / (sigma*sqrt(t));
d2 = (log(S/K) + (r-pow(sigma,2)/2)*t) / (sigma*sqrt(t));
val = R::pnorm(d1,0.0,1.0)*S - R::pnorm(d2,0.0,1.0)*K*exp(-r*t);
return val;
}
