#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


// [[Rcpp::export]]
double loglikelihood(NumericVector yobs,
                     NumericVector xind, 
                     double a, double b, double s){
  
  int n = yobs.size();
  NumericVector veca(n); 
  veca = rep(a, n);
  NumericVector vecb(n);
  vecb= rep(b, n);
  NumericVector vecs(n);
  vecs= rep(s, n);
  
  NumericVector muis(n);
  
  for(int i=0; i < n; ++i){
    muis[i]=a+b*xind[i];
  }
  
  NumericVector out(n);
  double aux;
  aux=0;
  for(int i = 0; i < n; ++i) {
    out[i] = R::dnorm( yobs[i], muis[i], s, true);
    aux=out[i]+aux;
  }
   double loglike ;
  loglike= aux;
  return loglike;
}


// [[Rcpp::export]]
double logprior(NumericVector yobs,
                double mua, double sa,
                double mub, double sb,
                double sh, double rt){
  int n = yobs.size();

  double logpriora = R::dunif(1, 0, 5, true) ;
  double logpriorb = R::dunif(.5, 0, 1, true) ;
  double logpriors = R::dunif(.5, 0, 1, true) ;
  return logpriora + logpriorb + logpriors;
}

// [[Rcpp::export]]
double logposterior(NumericVector yobs,
                    NumericVector xind, 
                    double a, double b, double s, 
                    double mua, double sa,
                    double mub, double sb,
                    double sh, double rt){
  
  double loglk;
  
  loglk = loglikelihood(yobs, xind, a, b, s);
  
  double logpr;
  
  logpr = logprior(yobs, mua, sa, mub, sb, sh, rt);
  
  return  loglk + logpr ;
  
}
