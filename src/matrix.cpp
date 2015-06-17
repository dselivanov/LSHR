#include <Rcpp.h>
#include <unordered_map>
// Enable C++11
// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
vector< vector<int> >  get_tdm(List sets) {
  vector< vector< int > > res( sets.size() );
  unordered_map < string, int> dict;
  unordered_map < string, int > :: const_iterator element_it;
  int k = 0;
  for (auto it : sets) {
    checkUserInterrupt();
    CharacterVector current_set = it;
    int K = current_set.size();
    int j = 0;
    vector< int > indices(K);
    for (auto element : current_set) {
      element_it = dict.find(as<string>(element));
      int key;
      if(element_it == dict.end()) {
        key = dict.size() + 1;
        dict.insert(make_pair(as<string>(element), key));
      } else {
        key = element_it -> second;
      }
      indices[j] = key;
      j++;
    }
    res[k] = indices;
    k++;
  }
  return(res);
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
get_tdm(list(letters[1:5], letters[3:7], letters[8:10]))
*/
