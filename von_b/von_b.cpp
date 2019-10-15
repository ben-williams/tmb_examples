// von Bertalanffy growth curve
// data inputs are individual ages and lengths
// ben.williams@alaska.gov

#include <TMB.hpp>
template<class Type>

Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(age);
  DATA_VECTOR(length);
  int n = age.size(); // declare and specify an integer n the length of y
  
  PARAMETER(Linf);
  PARAMETER(kappa);
  PARAMETER(t0)
    PARAMETER(log_sigma);
  
  // procedures (transformed parameters)
  Type sigma = exp(log_sigma);
  
  // create a vector for predictions
  vector<Type> yfit(n); 
  
  // negative log likelihood 
  
  Type nll = 0.0; // set nll to 0
  
  for(int i=0; i<n; i++){
    yfit(i) = Linf * (1.0 - exp(-kappa * (age(i) - t0)));
  }
  
  nll = -sum(dnorm(length, yfit, sigma, true));
  

  REPORT(Linf);
  REPORT(kappa);
  REPORT(t0);
  REPORT(sigma);
  
  return nll;
  
}
