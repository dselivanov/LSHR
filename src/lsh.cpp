#include "LSHR.h"
// [[Rcpp::export]]
IntegerMatrix hash_signatures(IntegerMatrix m, int bands_number, int rows_per_band) {

  // IntegerMatrix sketch_hashes(bands_number, m.ncol());
  IntegerMatrix sketch_hashes( m.ncol(), bands_number);

  for (int i = 0; i < m.ncol(); i++) {
    int index_row = 1;
    for(int j = 0; j < bands_number; j++) {
      int hash = 0;
      for (int k = 0; k < rows_per_band; k++) {
        // sketch_hashes(j, i) =  atom_hashfun_1( m(j * rows_per_band + k, i) )  * (index_row + 1);
        //hash +=  atom_hashfun_1( m(j * rows_per_band + k, i) )  * (index_row + 1);
        hash +=   atom_hashfun_1(m(j * rows_per_band + k, i)  * (index_row + 1));
        index_row++;
      }
      sketch_hashes(i, j) = hash;
    }
  }
  return(sketch_hashes);
}

// [[Rcpp::export]]
DataFrame pairs(ListOf<IntegerVector> lst) {

  unordered_map < pair<uint32_t, uint32_t>, uint32_t> res;
  IntegerVector x;
  for(int k = 0; k < lst.size(); k++) {
    x = lst[k];
    for(int i = 0; i < x.size() - 1; i++) {
      for(int j = i + 1; j < x.size(); j++) {
        res[make_pair(x[i], x[j])]++;
      }
    }
  }

  // construct result
  //IntegerMatrix m(res.size(), 3);
  IntegerVector id1(res.size());
  IntegerVector id2(res.size());
  IntegerVector count(res.size());
  int i = 0;
  for(auto it: res) {
    id1[i] = it.first.first;
    id2[i] = it.first.second;
    count[i] = it.second;
    i++;
  }

  //shrink memory usage
  unordered_map<pair<uint32_t, uint32_t>, uint32_t>().swap(res);
  return DataFrame::create(_["id1"] = id1,
                           _["id2"] = id2);
}
