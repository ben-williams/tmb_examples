// double logistic selectivity
// data inputs are individual ages and lengths
// ben.williams@alaska.gov

#include<TMB.hpp>
template<class Type>

Type objective_function<Type>::operator()()
{
	// data -----
	DATA_VECTOR(X); // can be age or length
	DATA_VECTOR(prop); // proportion range (0-1) of presence of X
	int n = X.size(); // declare and specify an integer n the length of X

	// parameters ----

	PARAMETER(inf);
	PARAMETER(inf2);
	PARAMETER(slope);
	PARAMETER(slope2);
	PARAMETER(max_sel);
	PARAMETER(min_sel);
	PARAMETER(log_sigma);

	// procedures (transform parameters) -----

	Type sigma = exp(log_sigma);
    //min_sel = 1 - min_sel;
	// create vector for predictions

	vector<Type> fit(n);

	// negative log likelihood - set to 0 ----

	Type nll = 0.0;

	for(int i=0; i<n; i++){
		fit(i) = (max_sel / (Type(1.0) + exp(-slope * (X(i) - inf))))*
		(Type(1.0) - min_sel / (Type(1.0) + exp(-slope2 * (X(i) - inf2))));
	} 

	nll = -sum(dnorm(prop, fit, sigma, true));

	// reports ----

	REPORT(inf);
	REPORT(inf2);
	REPORT(slope);
	REPORT(slope2);
	REPORT(max_sel);
	REPORT(min_sel);
	REPORT(log_sigma);

	return nll;
}
