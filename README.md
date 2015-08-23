# Locality Sensitive Hashing in R 
LSHR - fast and memory efficient package for near-neighbor search in high-dimensional data. Two LSH schemes implemented at the moment:

1. Minhashing for jaccard similarity
2. Sketching (or random projections) for cosine similarity.
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
# create sparse term-document matrix (in the list-of-lists form)
dtm <- create_dtm(sets, format = 'lil')
jaccard_sign_mat <- get_signature_matrix(dtm, hashfun_number = 100, measure = 'jaccard',cores =  1)
# find close pairs of sets
candidate_indices <- get_similar_pairs(signature_matrix = jaccard_sign_mat,
                                         bands_number = 10,
                                         similarity = 0.8,
                                         verbose = T)

```
