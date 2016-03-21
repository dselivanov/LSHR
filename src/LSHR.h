#include <Rcpp.h>
//// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
//// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace std;
#include <Rcpp.h>
#include <string>
#include <vector>
#include <unordered_map>

// for unordered_map < <uint32_t, uint32_t>, T >
namespace std {
template <>
struct hash<std::pair<uint32_t, uint32_t>>
{
  inline uint64_t operator()(const std::pair<uint32_t, uint32_t>& k) const
  {
    //should produce no collisions
    //http://stackoverflow.com/a/24693169/1069256
    //http://stackoverflow.com/questions/2768890/how-to-combine-two-32-bit-integers-into-one-64-bit-integer?lq=1
    return (uint64_t) k.first << 32 | k.second;
  }
};
}

uint32_t atom_hashfun_1( uint32_t a);
uint32_t atom_hashfun_2( uint32_t a);
