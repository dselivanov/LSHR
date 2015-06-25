#include <Rcpp.h>
// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]
// Enable OpenMP
// [[Rcpp::plugins(openmp)]]

#ifdef SUPPORT_OPENMP
  #include <omp.h>
#endif

#include <unordered_map>

using namespace Rcpp;
using namespace std;
