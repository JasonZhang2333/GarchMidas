//Includes/namespaces
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calculate_g(double omega, double alpha, double beta, NumericVector returns, double g0) {
  int n = returns.size();

  NumericVector g(n);
  g[0] = g0;
  for (int i = 1; i < n; i++) {
    g[i] = omega + alpha * returns[i-1]*returns[i-1] + beta * g[i-1];
  }
  return g;
}
