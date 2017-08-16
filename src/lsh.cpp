#include "LSHR.h"
// [[Rcpp::export]]
IntegerMatrix hash_signatures(IntegerMatrix m, int bands_number, int rows_per_band) {
  IntegerMatrix sketch_hashes( m.ncol(), bands_number);
  for (int i = 0; i < m.ncol(); i++) {
    int index_row = 1;
    for(int j = 0; j < bands_number; j++) {
      int hash = 0;
      for (int k = 0; k < rows_per_band; k++) {
        hash +=   atom_hashfun_1(m(j * rows_per_band + k, i)  * (index_row + 1));
        index_row++;
      }
      sketch_hashes(i, j) = hash;
    }
  }
  return(sketch_hashes);
}
