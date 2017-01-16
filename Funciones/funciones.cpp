#include <Rcpp.h>
using namespace Rcpp;
////funcion basada en el ejemplo de harting

// [[Rcpp::export]]
double getpi() {
  return M_PI; ///numero pi auxiliar
  }

// [[Rcpp::export]]
double loglikelihood(NumericVector theta, 
                     NumericVector X, 
                     NumericVector Y){
  double a = theta[0];
  double b = theta[1];
  double sd = theta[2];
  int n = X.length();
  
  NumericVector y2(n);
  for(int i=0; i < n; i++){
    y2[i] = a*X[i] + b ;
  }

  NumericVector auxLikelihood(n);
  double num;
  double den;
  
  for (int i=0; i < n; i++){
    num = exp(-pow((Y[i]-y2[i]), 2.0)/(2*sd*sd));
    den = sd*sqrt(2*getpi());
    auxLikelihood[i] = log(num/den);
  }
  
  double auxSuma = sum(auxLikelihood);
  return (auxSuma);
}

// [[Rcpp::export]]
double logPriori(NumericVector theta, 
                 double lsa,
                 double mub,
                 double sdb,
                 double lss){
  ///
  double a = theta[0];
  double b = theta[1];
  double sd = theta[2];
  ////
  double a2 = R::dunif(a, 0, lsa, true);
  double b2 = R::dnorm(b, mub, sdb, true);
  double sd2 = R::dunif(sd, 0, lss,true);
  
  double aux = a2+b2+sd2;
  return(aux);
}

// [[Rcpp::export]]
double logPosteriori(NumericVector theta, 
                     NumericVector X, 
                     NumericVector Y, 
                     double lsa2,
                     double mub2,
                     double sdb2,
                     double lss2){
  double aux1 = loglikelihood(theta, X, Y);
////
  double aux2 = logPriori(theta,lsa2,mub2,sdb2,lss2);
  aux2 = aux2 + aux1;
  return(aux2);
}

// [[Rcpp::export]]
NumericVector proposal(NumericVector theta){
  double a = theta[0];
  double b = theta[1];
  double sd = theta[2];
  
  double v1 = R::rnorm(a,  0.1);
  double v2 = R::rnorm(b,  0.5);
  double v3 = R::rnorm(sd, 0.3);
  
  NumericVector aux(3);
  aux[0] = v1;
  aux[1] = v2;
  aux[2] = v3;
  return (aux);
}

// [[Rcpp::export]]
NumericMatrix runMCMC(NumericVector x, NumericVector y, NumericVector startValue, int iterations,
                      double lsa3,
                      double mub3,
                      double sdb3,
                      double lss3){
  NumericMatrix chain(iterations+1, 3);
  for(int i=0; i < startValue.length(); i++){
    chain(0,i) = startValue[i];
  }
  
  NumericVector prop(3);
  NumericVector aux(3);
  double probab;
  for(int i=0; i < iterations; i++){
    for(int j=0; j < 3; j++){
      aux[j] = chain(i, j);
    }
    prop = proposal(aux);
    probab = exp(logPosteriori(prop, x, y, lsa3,mub3,sdb3,lss3) - logPosteriori(aux, x, y, lsa3,mub3,sdb3,lss3));
    if(R::runif(0,1) < probab){
      for(int j=0; j < startValue.length(); j++){
        chain(i+1, j) = prop[j];
      }
    }else{
      for(int j=0; j < startValue.length(); j++){
        chain(i+1, j) = chain(i,j);
       }
      }
    }
    return(chain);
}
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 