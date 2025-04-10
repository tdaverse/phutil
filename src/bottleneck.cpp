#include "hera/bottleneck.h"
#include "diagram_parser.h"

[[cpp11::register]]
double bottleneckDistance(const cpp11::doubles_matrix<>& x,
                          const cpp11::doubles_matrix<>& y,
                          const double delta = 0.01)
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
  cpp11::stop(msg.c_str());
}
