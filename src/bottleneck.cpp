#include "hera/bottleneck.h"
#include "diagram_parser.h"

double bottleneckDistExact(const PairVector& diagramA, const PairVector& diagramB, int decPrecision, hera::bt::MatchingEdge<double>& e, bool verbose = false)
{
  return hera::bottleneckDistExact(diagramA, diagramB, decPrecision, e, verbose);
}

// [[Rcpp::export]]
double bottleneckDistExact(const Rcpp::NumericMatrix& x, const Rcpp::NumericMatrix& y, int decPrecision, bool verbose = false)
{
  PairVector diagramA, diagramB;
  parseMatrix(x, diagramA);
  parseMatrix(y, diagramB);
  hera::bt::MatchingEdge<double> e;
  return bottleneckDistExact(diagramA, diagramB, decPrecision, e, verbose);
}

double bottleneckDistApprox(const PairVector& diagramA, const PairVector& diagramB, double delta, hera::bt::MatchingEdge<double>& e, bool verbose = false)
{
  return hera::bottleneckDistApprox(diagramA, diagramB, delta, e, verbose);
}

// [[Rcpp::export]]
double bottleneckDistApprox(const Rcpp::NumericMatrix& x, const Rcpp::NumericMatrix& y, double delta, bool verbose = false)
{
  PairVector diagramA, diagramB;
  parseMatrix(x, diagramA);
  parseMatrix(y, diagramB);
  hera::bt::MatchingEdge<double> e;
  return bottleneckDistApprox(diagramA, diagramB, delta, e, verbose);
}
