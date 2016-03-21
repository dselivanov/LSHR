#include "LSHR.h"
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
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector hashfun_1(IntegerVector vec) {
  int K = vec.size();
  Rcpp::IntegerVector res(K);
  for (int i = 0; i < K; i++)
    res[i] = atom_hashfun_1(vec[i]);
  return res;
}

Rcpp::IntegerVector hashfun_2(IntegerVector vec) {
  int K = vec.size();
  Rcpp::IntegerVector res(K);
  for (int i = 0; i < K; i++)
    res[i] = atom_hashfun_2(vec[i]);
  return res;
}

//' @export
// [[Rcpp::export]]
IntegerVector get_minhash_matrix(uint32_t unique_shingles_length, uint32_t hashfun_number, uint32_t seed) {
  IntegerMatrix res_matrix( hashfun_number, unique_shingles_length);
  uint32_t h1, h2, k;
  for (uint32_t i = 0; i < unique_shingles_length; i++) {
    k = i + 1 + seed;
    h1 = atom_hashfun_1(k);
    h2 = atom_hashfun_2(k);
    // we can generate as many independent hash functions as we want
    // http://stackoverflow.com/questions/24676237/generating-random-hash-functions-for-lsh-minhash-algorithm
    // http://www.eecs.harvard.edu/~kirsch/pubs/bbbf/rsa.pdf
    for (uint32_t j = 0; j < hashfun_number; j++) {
      res_matrix(j, i) = h1 + (j + 1) * h2 + j * j;
    }
  }
  return res_matrix;
}

