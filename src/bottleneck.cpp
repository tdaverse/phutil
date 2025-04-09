#include "hera/bottleneck.h"
#include "diagram_parser.h"

//' Bottleneck distance between two persistence diagrams
//'
//' This function computes the Bottleneck distance between two persistence
//' diagrams of the same homology dimension. The diagrams must be represented as
//' 2-column matrices. The first column of the matrix contains the birth times
//' and the second column contains the death times of the points.
//'
//' @param x A matrix of shape \eqn{n \times 2} specifying the first persistence
//'   diagram.
//' @param y A matrix of shape \eqn{m \times 2} specifying the second
//'   persistence diagram.
//' @param delta A numeric value specifying the relative error. Defaults to
//'   `0.0` in which case the exact Bottleneck distance is computed. If `delta`
//'   is greater than `0.0`, an approximate Bottleneck distance is computed.
//'
//' @return A numeric value storing the Bottleneck distance between the two
//'   persistence diagrams.
//'
//' @examples
//' bottleneck_distance(
//'   persistence_sample[[1]]$pairs[[1]],
//'   persistence_sample[[2]]$pairs[[1]]
//' )
//'
//' @export
// [[Rcpp::export]]
double bottleneck_distance(const Rcpp::NumericMatrix& x,
                           const Rcpp::NumericMatrix& y,
                           const double delta = 0.0)
{
  PairVector diagramA, diagramB;
  parseMatrix(x, diagramA);
  parseMatrix(y, diagramB);
  hera::bt::MatchingEdge<double> e;

  if (delta > 0.0)
  {
    return hera::bottleneckDistApprox(diagramA, diagramB, delta, e, true);
  }

  if (delta == 0.0)
  {
    int decPrecision { 0 };
    return hera::bottleneckDistExact(diagramA, diagramB, decPrecision);
  }

  std::string msg = "delta was \"" +
    std::to_string(delta) +
    "\", must be a number >= 0.0. Cannot proceed.";
  Rcpp::stop(msg.c_str());
}
