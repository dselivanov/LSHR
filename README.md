# Locality Sensitive Hashing in R 
LSHR - fast and memory efficient package for near-neighbor search in high-dimensional data. Two LSH schemes implemented at the moment:

1. Minhashing for jaccard similarity
2. Sketching (or random projections) for cosine similarity.
Most of ideas are based on brilliant [Mining of Massive Datasets](http://www.mmds.org) book. 

# Materials

* [Slides](http://www.slideshare.net/MailRuGroup/okru-finding-similar-items-in-highdimensional-spaces-locality-sensitive-hashing) (in english) and [video](https://youtu.be/ko0a0Z75oZQ?list=PLcJ8pdaABCSk1dNtpgaHvuV5y2gWItuUO) (in russian) from my talk at Moscow Data Science meetup.

# Quick reference
```R
# devtools::install_github('dselivanov/text2vec')
library(text2vec)
library(LSHR)
data("movie_review")
it <- itoken(movie_review$review, preprocess_function = tolower, tokenizer = word_tokenizer)
dtm <- create_dtm(it, hash_vectorizer())

hashfun_number = 120
get_s_curve(hashfun_number, n_bands_min = 5, n_rows_per_band_min = 5)
```
![S-curves](https://cloud.githubusercontent.com/assets/5123805/13917531/82d5162a-ef72-11e5-8f5a-59a8d1f1f729.png)
```R
options( mc.cores = 4)
system.time(sign_mat <- get_signature_matrix(dtm, hashfun_number = hashfun_number, measure = 'jaccard', seed = 12L))

candidate_indices <- get_similar_pairs(signature_matrix = sign_mat,
                                       bands_number = 5,
                                       verbose = T)
candidate_indices                                       
#    id1  id2
#1: 1291 1356
#2: 1615 3846
#3: 1054 1417
#4: 1084 3462
#5: 2805 4763
```
