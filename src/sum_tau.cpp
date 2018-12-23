//Includes/namespaces
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(name = "sum_tau")]]
NumericVector sum_tau(double m, double theta, NumericVector phivar, NumericVector covariate, int K) {

  int n = covariate.size() - K;
  NumericVector exponential(n);

  for (int i = 1; i <= n; i++) {
    exponential[i-1] = m;
    for (int j = 1; j <= K; j++) {
      exponential[i-1] += theta * phivar[j-1] * covariate[K + i - 1 - j];
    }
  }
  return exponential;
}
