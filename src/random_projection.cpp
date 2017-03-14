#include "LSHR.h"
using namespace Rcpp;

int omp_thread_count() {
  int n = 0;
  #ifdef _OPENMP
  #pragma omp parallel reduction(+:n)
  #endif
  n += 1;
  return n;
}

// [[Rcpp::export]]
IntegerVector project_spmat(const S4 &m, int n, int hash_fun_id_offest, int n_threads = 0) {
  int num_threads = n_threads;
  if(num_threads == 0) num_threads = omp_thread_count();
  IntegerVector dims = m.slot("Dim");
  // number of sample in current mini-batch
  int N = dims[0];
  IntegerVector PP = m.slot("p");
  int *P = PP.begin();
  IntegerVector JJ = m.slot("j");
  int *J = JJ.begin();
  NumericVector XX = m.slot("x");
  double *X = XX.begin();

  IntegerVector res(N);
  int *res_ptr = res.begin();

  #ifdef _OPENMP
  #pragma omp parallel for num_threads(num_threads)
  #endif
  for (int i = 0; i < N; i++) {
    uint32_t h1, h2;
    int p1 = P[i];
    int p2 = P[i + 1];
    vector<float> row(32);

    for(int k = p1; k < p2; k++) {
      int j = J[k];
      double x = X[k];
      h1 = atom_hashfun_1(j);
      h2 = atom_hashfun_2(j);
      #ifdef _OPENMP
      #pragma omp simd
      #endif
      for(uint hh = 0; hh < n; hh++) {
        int hh2 = hh + hash_fun_id_offest;
        uint32_t h = atom_hashfun_1((h1 + (hh2 + 1) * h2 + hh2 * hh2));
        row[hh] += ((int)h * x);
      }
    }
    bitset<32> bitrow;
    for(int hh = 0; hh < n; hh++) {
      if(row[hh] < 0)
        bitrow[hh] = 0;
      else
        bitrow[hh] = 1;
    }
    res_ptr[i] = bitrow.to_ulong();
  }
  return res;
}

