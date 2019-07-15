
#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericVector colMaxRcpp(NumericMatrix X) {
  int ncol = X.ncol();
  Rcpp::NumericVector out(ncol);
  for (int col = 0; col < ncol; col++){
    out[col]=Rcpp::max(X(_, col)); 
  } 
  return wrap(out);
} 

