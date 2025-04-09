#include "hera/wasserstein.h"
#include "diagram_parser.h"

double wassersteinDist(PairVector& diagramA,
                       PairVector& diagramB,
                       const double wasserstein_power = 1.0,
                       const double delta = 0.01,
                       const double internal_p = hera::get_infinity<double>(),
                       const double initial_epsilon = 0.0,
                       const double epsilon_common_ratio = 5.0,
                       const int max_bids_per_round = 1,
                       const int max_num_phases = std::numeric_limits<decltype(max_num_phases)>::max(),
                       const bool tolerate_max_iter_exceeded = false,
                       const bool return_matching = false,
                       const bool match_inf_points = true,
                       const bool print_relative_tolerance = false,
                       const bool verbose = false)
{
  using Real = double;
  hera::AuctionParams<Real> params;
  params.wasserstein_power = wasserstein_power;
  params.delta = delta;
  params.internal_p = internal_p;
  params.initial_epsilon = initial_epsilon;
  params.epsilon_common_ratio = epsilon_common_ratio;
  params.max_bids_per_round = max_bids_per_round;
  params.max_num_phases = max_num_phases;
  params.tolerate_max_iter_exceeded = tolerate_max_iter_exceeded;
  params.return_matching = return_matching;
  params.match_inf_points = match_inf_points;

  if (params.wasserstein_power < 1.0) {
    std::string msg = "Wasserstein_degree was \"" +
      std::to_string(params.wasserstein_power) +
      "\", must be a number >= 1.0. Cannot proceed.";
    Rcpp::stop(msg.c_str());
  }

  if (params.wasserstein_power == 1.0) {
    hera::remove_duplicates<Real>(diagramA, diagramB);
  }

  if (params.delta <= 0.0) {
    std::string msg = "relative error was \"" +
      std::to_string(params.delta) +
      "\", must be a number > 0.0. Cannot proceed.";
    Rcpp::stop(msg.c_str());
  }

  // default for internal metric is l_infinity
  if (std::isinf(params.internal_p)) {
    params.internal_p = hera::get_infinity<Real>();
  }

  if (not hera::is_p_valid_norm<Real>(params.internal_p)) {
    std::string msg = "internal-p was \"" +
      std::to_string(params.internal_p) +
      "\", must be a number >= 1.0 or inf. Cannot proceed.";
    Rcpp::stop(msg.c_str());
  }

  // if you want to specify initial value for epsilon and the factor
  // for epsilon-scaling
  if (params.initial_epsilon < 0.0) {
    std::string msg = "initial-epsilon was \"" +
      std::to_string(params.initial_epsilon) +
      "\", must be a non-negative number. Cannot proceed.";
    Rcpp::stop(msg.c_str());
  }

  if (params.epsilon_common_ratio <= 1.0 and params.epsilon_common_ratio != 0.0) {
    std::string msg = "epsilon-common-ratio was \"" +
      std::to_string(params.epsilon_common_ratio) +
      "\", must be a number > 1.0 or 0.0. Cannot proceed.";
    Rcpp::stop(msg.c_str());
  }

  if (params.max_bids_per_round == 0)
    params.max_bids_per_round = std::numeric_limits<decltype(params.max_bids_per_round)>::max();

  auto res = hera::wasserstein_cost_detailed(diagramA, diagramB, params);

  if (print_relative_tolerance)
    Rcpp::Rcout << "Relative tolerance: " << res.final_relative_error << std::endl;

  return res.cost;
}

//' Wasserstein distance between two persistence diagrams
//'
//' This function computes the Wasserstein distance between two persistence
//' diagrams of the same homology dimension. The diagrams must be represented as
//' 2-column matrices. The first column of the matrix contains the birth times
//' and the second column contains the death times of the points.
//'
//' @param x A matrix of shape \eqn{n \times 2} specifying the first persistence
//'   diagram.
//' @param y A matrix of shape \eqn{m \times 2} specifying the second
//'   persistence diagram.
//' @param wasserstein_power A numeric value specifying the power of the
//'   Wasserstein distance. Defaults to `1.0`.
//' @param delta A numeric value specifying the relative error. Defaults to
//'   `0.01`.
//'
//' @return A numeric value storing the Wasserstein distance between the two
//'   persistence diagrams.
//'
//' @examples
//' wasserstein_distance(
//'   persistence_sample[[1]]$pairs[[1]],
//'   persistence_sample[[2]]$pairs[[1]]
//' )
//'
//' @export
// [[Rcpp::export]]
double wasserstein_distance(const Rcpp::NumericMatrix& x,
                            const Rcpp::NumericMatrix& y,
                            const double wasserstein_power = 1.0,
                            const double delta = 0.01)
{
  PairVector diagramA, diagramB;
  parseMatrix(x, diagramA);
  parseMatrix(y, diagramB);
  return wassersteinDist(diagramA, diagramB, wasserstein_power, delta);
}
