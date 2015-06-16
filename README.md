# Locality Sensitive Hashing in R 
LSHR - fast and memory efficient package for reducing the dimensionality of high-dimensional data. Only **minhashing** implemented at the moment.
Most of ideas are based on brilliant [Mining of Massive Datasets](http://www.mmds.org) book - many thanks to authors. 

# Quick reference
```R
set.seed(10)
library(LSHR)
# generate element of sets
elems <- sapply(1:100, function(z) 
  paste(sample(letters, sample(3:7), replace = T), collapse=''))
# generate sets
sets <-  lapply(1:100, function(z) sample(elems, sample(10:40)))
# add near-duplicates
sets <- c(sets, lapply(sets[1:10], function(x) c(x, sample(elems, 5))  ))
m <- get_signature_matrix(sets = sets, hashfun_number = 60, cores = 2)
candidate_indices <- get_candidate_pairs(signature_matrix = m, 
                                         bands_number = 10, similarity = 0.8, verbose = T)
candidate_indices
```
