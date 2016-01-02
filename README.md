# Locality Sensitive Hashing in R 
LSHR - fast and memory efficient package for near-neighbor search in high-dimensional data. Two LSH schemes implemented at the moment:

1. Minhashing for jaccard similarity
2. Sketching (or random projections) for cosine similarity.
Most of ideas are based on brilliant [Mining of Massive Datasets](http://www.mmds.org) book - many thanks to authors. 

# Quick reference
```R
# devtools::install_github('dselivanov/text2vec')
library(text2vec)
library(LSHR)
data("movie_review")

it <- itoken(movie_review$review, preprocess_function = tolower, tokenizer = word_tokenizer)

# create document-term matrix using hashing trick vectorizer
corp <- create_hash_corpus(it, feature_hasher = feature_hasher(hash_size = 2^14, ngram = c(1,1)))
dtm <- get_dtm(corp)

jaccard_sign_mat <- get_signature_matrix(dtm, hashfun_number = 120, measure = 'jaccard', seed = 1L, mc.cores=4)

candidate_indices <- get_similar_pairs(signature_matrix = jaccard_sign_mat,
                                       bands_number = 5,
                                       verbose = T)
candidate_indices
```
