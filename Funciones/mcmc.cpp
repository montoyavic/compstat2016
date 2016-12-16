#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]

double loglikelihood(double sig, double prior_a, double prior_b,NumericVector y, NumericVector x) {
  NumericVector pred = prior_a + prior_b*x ;
  double loglik = 0;
 for (int i=0; i < y.length(); i++) {
   NumericVector l = dnorm(y, pred[i],sig,true);
   loglik = loglik + l[i] ;
  }
 return loglik;
}

// [[Rcpp::export]]
double logprior(NumericVector prior_a, NumericVector prior_b, NumericVector sig){
  
  NumericVector aprior = dunif(prior_a,0.0,170.4,true);
  NumericVector bprior = dnorm(prior_b,10,true);
  NumericVector sdprior = dunif(sig,0,30,true);
  return(aprior[0]+bprior[0]+sdprior[0]);
}

//[[Rcpp::export]]

double logposterior(double sig, double prior_a, double prior_b,NumericVector y, NumericVector x, NumericVector prior_a2, NumericVector prior_b2, NumericVector sig2) {
 return(loglikelihood(sig,prior_a,prior_b,y,x)+logprior(prior_a2,prior_b2,sig2));
}


//[[Rcpp::export]]
NumericVector run_mcmc(
    int n_sim,
    double start_v,
    double jump,
    double sig, 
    double prior_a, 
    double prior_b,
    NumericVector x,
    NumericVector y,
    NumericVector prior_a2,
    NumericVector prior_b2, 
    NumericVector sig2) {
  
  NumericVector sim(n_sim + 1); // aqui voy a guardar las simulaciones
  sim[0] = start_v;
  double U, eta;
  bool accepted;
  for (int i=0; i < n_sim; i++) {
    // do while hasta que acepte el candidato
    do {
      eta = (rnorm(3, sim[i], jump))[0]; // genera el candidato
      U = (runif(1))[0];
      if (eta < 0 || eta > 1) {
        accepted = false;
      } else {
        accepted = (log(U) <= logposterior(eta,prior_a, prior_b,y,x,prior_a2,prior_b2, eta) -
          logposterior(sim[i],prior_a, prior_b,y,x,prior_a2,prior_b2, sim[i]));
      }
    } while (!accepted);
    sim[i + 1] = eta;
  }
  return sim;
}
