/*
 * Rcpp function for identifying sequences of potential
 * privatization episodes (increasing v2clstown_osp).
 *
 * NOTE: the function argument "cum_incl" is only set and implemented
 * in the R script and checks if potential episodes are "manifest"
 */

#include <Rcpp.h>
#include <queue>

using namespace Rcpp;
using namespace std;

//' Identify sequences of potential privatization episodes
//'
//' This is a subfunction (c++) of ESOT::get_eps see the
//' documentation of ESOT::get_eps (?get_eps) for details on
//' parameters, etc.
//'
//[[Rcpp::export]]
NumericVector find_seqs_priv(NumericVector v,
                            NumericVector r,
                            NumericVector t,
                            double start_incl = 0.04,
                            double year_turn = -0.12,
                            double cum_turn = -0.4,
                            int tolerance = 5) {
  if (v.size() != r.size() || v.size() != t.size())
    stop("Mismatched vector lengths");

  if (start_incl < 0 || year_turn > 0 || cum_turn > 0)
    stop("start_incl must be positive and year_turn and cum_turn negative");

  if (tolerance <= 0)
    stop("Tolerance threshold must be greater than zero");

  NumericVector out = NumericVector(v.size(), NumericVector::get_na()),
    d = diff(v), r2 = diff(r);

  // Setting up the objects that keep track of the country-specific
  // episode number (count), the number of years of stasis there has
  // been (tolerance_count) and the cumulative change during the time
  // of stasis
  queue<int> q;
  int count = 1, tolerance_count = 0;
  double change = 0.000;
  size_t d_len = d.size();

  // Start of a loop that looks for the beginning of an episode
  // (d[i] >= start_incl). Records the index of the d value that is
  // greater than start_incl in q. resets the tolerance count and
  // total diff to zero since this if() statement only occurs at the
  // beginning of an episode.
  for (size_t i = 0; i < d_len; i++) {
    if (d[i] >= start_incl) {
      q.push(i);
      tolerance_count = 0;
      change = 0;
    }

    // We haven't found the start of a sequence (q.empty() == T), keep
    // going
    if (q.empty())
      continue;

    // Increase the tolerance_count and the change count if there is
    // stasis
    if (year_turn <= d[i] && d[i] < start_incl) {
      tolerance_count++;
      change += d[i];
    }

    // How do we end a seq? Either:
    //  - End of vector
    //  - Reach tolerance w/o another inc
    //  - Hit a NA
    //  - Decrease < cum_turn
    //  - Revert to econ_type == 0 (return to planned economy)
    if (i == d_len - 1 || tolerance_count == tolerance || NumericVector::is_na(d[i]) ||
        d[i] < year_turn|| change < cum_turn || (r2[i] < 0 && r[i+1] == 0) ||
        t[i+1] == -1) {
      int head = q.front(), tail;

      // Include stasis period
      if (tolerance_count > 0)
        tail = (i == d_len - 1 && d[i] >= year_turn) ? i + 1 : i;
      else
        tail = (q.size() > 1) ? q.back() + 1 : head + 1;

      NumericVector sub = NumericVector(tail - head + 1);
      sub.fill(count);

      out[seq(head, tail)] = sub;

      count++;
      tolerance_count = 0;
      change = 0;

      queue<int> empty;
      swap(q, empty);
    }
  }

  return out;
}
