#include <Rcpp.h>

using PairVector = std::vector<std::pair<double,double>>;

void parseMatrix(const Rcpp::NumericMatrix& matrix, PairVector& result);
