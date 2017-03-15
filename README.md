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
dtm = as(dtm, "RsparseMatrix")

hashfun_number = 120
s_curve <- get_s_curve(hashfun_number, n_bands_min = 5, n_rows_per_band_min = 5)
# Examine S-curve.
# Find tradeoff between accuracy and false-positive rate.
```
![S-curves](https://cloud.githubusercontent.com/assets/5123805/13917531/82d5162a-ef72-11e5-8f5a-59a8d1f1f729.png)
```R
seed = 1
pairs = get_similar_pairs(dtm, bands_number = 10, rows_per_band = 32, distance = 'cosine', seed = seed)

pairs[order(-N)]

#        id1  id2  N
#    1: 1054 1417 10
#    2: 1084 3462 10
#    3: 1291 1356 10
#    4: 1615 3846 10
#    5: 2805 4763  4
#   ---             
# 2304: 4767 4961  1
# 2305: 4772 4776  1
# 2306: 4810 4859  1
# 2307: 4854 4945  1
# 2308: 4905 4918  1
```
