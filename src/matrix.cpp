#include "lshr.h"

template<class T>
void get_dtm(List sets, vector< vector<int> > &res, vector<T> &terms ) {
  unordered_map < T, int> dict;
  typename unordered_map < T, int > :: const_iterator element_it;
  int k = 0;
  for (auto it : sets) {
    //checkUserInterrupt();
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
        terms.push_back(element);
      } else {
        col_index = element_it -> second;
      }
      indices[j] = col_index;
      j++;
    }
    res[k] = indices;
    k++;
  }
}

template<class T>
List  get_dtm_r (List sets) {
  // list-of-lists matrix. Each vector will represent indices of temrs it contains.
  vector < vector< int > > docs(sets.size());
  // terms array = set of elements from all documents
  vector<T> terms;
  // construct matrix
  get_dtm< T >(sets, docs, terms);
  // wrap into R list
  List dtm_lil =  wrap(docs);
  // keep information about original terms
  dtm_lil.attr("terms") = terms;
  return dtm_lil;
}

//' @export
// [[Rcpp::export]]
List  get_dtm_character(List sets) {
  return get_dtm_r<string> (sets);
}

//' @export
// [[Rcpp::export]]
List  get_dtm_integer(List sets) {
  return get_dtm_r< int > (sets);
}
