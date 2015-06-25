#include "lshr.h"

// http://stackoverflow.com/a/12996028/1069256
uint32_t atom_hashfun_1(uint32_t a) {
  a = ((a >> 16) ^ a) * 0x45d9f3b;
  a = ((a >> 16) ^ a) * 0x45d9f3b;
  a = ((a >> 16) ^ a);
  return a;
}
// http://burtleburtle.net/bob/hash/integer.html
uint32_t atom_hashfun_2( uint32_t a) {
  a = (a ^ 61) ^ (a >> 16);
  a = a + (a << 3);
  a = a ^ (a >> 4);
  a = a * 0x27d4eb2d;
  a = a ^ (a >> 15);
  return a;
}

// [[Rcpp::export]]
Rcpp::IntegerVector hashfun_1(IntegerVector vec, int cores = 2) {
  int K = vec.size();
  Rcpp::IntegerVector res(K);
  #ifdef SUPPORT_OPENMP
    omp_set_num_threads(cores);
    #pragma omp parallel for schedule(static)
  #endif
  for (int i = 0; i < K; i++)
    res[i] = atom_hashfun_1(vec[i]);
  return res;
}

Rcpp::IntegerVector hashfun_2(IntegerVector vec, int cores = 2) {
  int K = vec.size();
  Rcpp::IntegerVector res(K);
  #ifdef SUPPORT_OPENMP
    omp_set_num_threads(cores);
    #pragma omp parallel for schedule(static)
  #endif
  for (int i = 0; i < K; i++)
    res[i] = atom_hashfun_2(vec[i]);
  return res;
}

//' @export
// [[Rcpp::export]]
IntegerMatrix get_hash_matrix(int unique_shingles_length, int hashfun_number = 60, int cores = 2) {
  IntegerMatrix res(unique_shingles_length, hashfun_number);
  #ifdef SUPPORT_OPENMP
    omp_set_num_threads(cores);
    #pragma omp parallel for schedule(static)
  #endif
  for (int i = 0; i < unique_shingles_length; i++) {
    uint32_t h1 = atom_hashfun_1(i + 1);
    uint32_t h2 = atom_hashfun_2(i + 1);
    int val;
    // we can generate as many independent hash functions as we want
    // http://stackoverflow.com/questions/24676237/generating-random-hash-functions-for-lsh-minhash-algorithm
    // http://www.eecs.harvard.edu/~kirsch/pubs/bbbf/rsa.pdf
    for (int j = 0; j < hashfun_number; j++) {
      val = h1 + (j + 1) * h2 + j * j;
      res(i, j) = val;
    }
  }
  return res;
}
