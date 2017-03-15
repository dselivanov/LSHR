#include "LSHR.h"
using namespace Rcpp;

// http://stackoverflow.com/a/12996028/1069256
inline uint32_t hash_1(uint32_t a) {
  a = ((a >> 16) ^ a) * 0x45d9f3b;
  a = ((a >> 16) ^ a) * 0x45d9f3b;
  a = ((a >> 16) ^ a);
  return a;
}
// http://burtleburtle.net/bob/hash/integer.html
inline uint32_t hash_2( uint32_t a) {
  a = (a ^ 61) ^ (a >> 16);
  a = a + (a << 3);
  a = a ^ (a >> 4);
  a = a * 0x27d4eb2d;
  a = a ^ (a >> 15);
  return a;
}

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
      h1 = hash_1(j);
      h2 = hash_2(j);
      #ifdef _OPENMP
      #pragma omp simd
      #endif
      for(uint hh = 0; hh < n; hh++) {
        int hh2 = hh + hash_fun_id_offest;
        uint32_t h = hash_1(h1 + h2 + hh2);
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

