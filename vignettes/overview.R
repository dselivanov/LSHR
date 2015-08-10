## ----quick-ref-----------------------------------------------------------
# set.seed(10)
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

## ------------------------------------------------------------------------
# for reproducible results
set.seed(seed = 17)
# we like %>% pipe operator! =)
library('magrittr')
# generate about 10000 "lastnames"
lastnames <- sapply(1:1e4, function(x) 
  sample(letters, sample(4:8)) %>% paste(collapse = '')) %>% 
  unique
print(head(lastnames))

## ------------------------------------------------------------------------
candidate_1_friends_set <- sample(lastnames, sample(20:150), replace = F)
candidate_2_friends_set <- sample(lastnames, sample(20:150), replace = F)

## ------------------------------------------------------------------------
jaccard <- function(x, y) {
  set_intersection <- length(intersect(x, y))
  set_union <- length(union(x, y))
  return(set_intersection / set_union)
}

