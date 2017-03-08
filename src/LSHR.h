#include <Rcpp.h>
using namespace Rcpp;
using namespace std;
#include <Rcpp.h>
#include <string>
#include <vector>
#include <unordered_map>

#ifdef _OPENMP
#include <omp.h>
#endif

uint32_t atom_hashfun_1( uint32_t a);
uint32_t atom_hashfun_2( uint32_t a);

int omp_thread_count();
