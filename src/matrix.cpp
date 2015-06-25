#include "lshr.h"
//
template<class T>
List  get_tdm(List sets) {
//vector< vector<int> >  get_tdm(List sets) {
  //vector< vector< int > > res( sets.size() );
  List res( sets.size() );
  unordered_map < T, int> dict;
  typename unordered_map < T, int > :: const_iterator element_it;
  int k = 0;
  for (auto it : sets) {
    checkUserInterrupt();
    vector<T> current_set = it;
    int K = current_set.size();
    int j = 0;
    vector< int > indices(K);
    for (auto element : current_set) {
      element_it = dict.find(element);
      int key;
      if(element_it == dict.end()) {
        key = dict.size() + 1;
        dict.insert(make_pair(element, key));
      } else {
        key = element_it -> second;
      }
      indices[j] = key;
      j++;
    }
    res[k] = indices;
    k++;
  }
  return res;
}

// [[Rcpp::export]]
List  get_tdm_character(List sets) {
  return get_tdm< string >(sets);
}
// [[Rcpp::export]]
List  get_tdm_integer(List sets) {
  return get_tdm< int >(sets);
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

// /*** R
// get_tdm_character(list(letters[1:5], letters[3:7], letters[8:10]))
// get_tdm_integer(list(1:5, 3:7, 8:11))
// */
