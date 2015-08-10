#include "lshr.h"

template<class T>
int get_tdm(List sets, vector< vector<int> > &res ) {
  unordered_map < T, int> dict;
  typename unordered_map < T, int > :: const_iterator element_it;
  int k = 0;
  for (auto it : sets) {
    checkUserInterrupt();
    vector<T> current_set = it;
    int K = current_set.size();
    int j = 0;
    int col_index;
    vector< int > indices(K);
    for (auto element : current_set) {
      element_it = dict.find(element);
      if(element_it == dict.end()) {
        col_index = dict.size() + 1;
        dict.insert(make_pair(element, col_index));
      } else {
        col_index = element_it -> second;
      }
      indices[j] = col_index;
      j++;
    }
    res[k] = indices;
    k++;
  }
  return dict.size();
}

template<class T>
List  get_tdm_r (List sets) {
  vector < vector< int > > res(sets.size());
  int dict_size = get_tdm< T >(sets, res);
  List tdm_lil =  wrap(res);
  tdm_lil.attr("ncol") = dict_size;
  return tdm_lil;
}

//' @export
// [[Rcpp::export]]
List  get_tdm_character(List sets) {
  return get_tdm_r<string> (sets);
}

//' @export
// [[Rcpp::export]]
List  get_tdm_integer(List sets) {
  return get_tdm_r< int > (sets);
}
