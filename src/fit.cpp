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
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}

// [[Rcpp::export]]
double mean_cpp(NumericVector x){
  int n = x.size();
  double sum = 0;
  for(int i = 0; i < n; i++){
    sum += x[i];
  }
  return sum/n;
}

//' Efficiently fit a simple linear regression in C++
//' @export
//' @param x Vector of covariate observations
//' @param y Vector of response observations
//' @returns An Rcpp List object containing model elements.
// [[Rcpp::export]]
List simp_lin_cpp(NumericVector x, NumericVector y){
// invariants: x and y are same length, var(x) > 0
  int n = x.size();
  double xbar = mean_cpp(x);
  double ybar = mean_cpp(y);
  
  //Coeficients
  double denom = 0;
  double numer = 0;
  for(int i = 0; i < n; i++){
    denom += pow(x[i] - xbar, 2);
    numer += (x[i] - xbar) * (y[i] - ybar);
  }
  double b1 = numer/denom;
  double b0 = ybar - b1 * xbar;
  
  //Predicted Values
  NumericVector yhat(n);
  NumericVector resid(n);
  double sse = 0;
  for(int i = 0; i < n; i++){
    yhat[i] = b0 + b1*x[i];
    resid[i] = y[i] - yhat[i];
    sse += pow(resid[i], 2);
  }
  double mse = sse/(n-2);
  
  // Inference
  double b1_se = sqrt(mse/(denom));
  double b0_se = sqrt(mse * (1.0/n  + pow(xbar, 2)/denom));
  NumericVector b1_ci(2);
  NumericVector b0_ci(2);
  double critval = R::qt(0.975, n-2, true, 0);
  b1_ci[0] = b1 - critval *b1_se;
  b1_ci[1] = b1 + critval *b1_se;
  b0_ci[0] = b0 - critval *b0_se;
  b0_ci[1] = b0 + critval *b0_se;
    
  List output = List::create(
    Named("b0") = b0,
    Named("b1") = b1,
    Named("b0_se") = b0_se,
    Named("b1_se") = b1_se,
    Named("b0_ci") = b0_ci,
    Named("b1_ci") = b1_ci,
    Named("resids") = resid,
    Named("yhat") = yhat
  );
  return output;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
mean_cpp(c(1, 2, 3, 4, 5))
x <- 1:10
y <- seq(2, 20, by=2) + c(-0.5, 0.45, 0.2, -0.1, 0.6, -0.3, 0.2, 0.6, 1.2, 0.2)
modR <- lm(y ~ x)
modC <- simp_lin_cpp(x, y)
z <- modR$coefficients
names(z) = NULL
assertthat::validate_that(all.equal(z[1], modC[["b0"]], tol=0.0001))
assertthat::validate_that(all.equal(z[2], modC[["b1"]], tol=0.0001))
assertthat::validate_that(all.equal(resid(modR), modC[["resids"]], tol=0.001, check.attributes = FALSE))
*/
