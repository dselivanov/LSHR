## ----quick-ref-----------------------------------------------------------
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

## ------------------------------------------------------------------------
library(microbenchmark)
timings <- microbenchmark(jaccard(candidate_1_friends_set, candidate_2_friends_set))
print(timings)
mean_timings <- mean(timings[['time']])
# convert from nano-seconds to seconds
mean_timings <- mean_timings * 1e-9

## ------------------------------------------------------------------------
mean_timings * 5000 * 5000 / 60

## ----toy-example-generate------------------------------------------------
set1 <- c('SMITH', 'JOHNSON', 'WILLIAMS', 'BROWN')
set2 <- c('SMITH', 'JOHNSON', 'BROWN')
set3 <- c('THOMAS', 'MARTINEZ', 'DAVIS')
set_list <- list(set1, set2, set3)

## ----toy-example-matrix-conctruction-1-----------------------------------
sets_dict <- unlist(set_list) %>% unique
m <- sapply(set_list, FUN = function(set, dict) as.integer(dict %in% set), dict = sets_dict, simplify = T)
dimnames(m) <- list(sets_dict, paste('set', 1:length(set_list), sep = '_'))

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(m)

## ----toy-example-matrix-similarity---------------------------------------
print(jaccard(set1, set2))

## ------------------------------------------------------------------------
column_jaccard <-  function(c1, c2) {
  non_zero <- which(c1 | c2)
  column_intersect <- sum(c1[non_zero] & c2[non_zero])
  column_union <- length(non_zero)
  return(column_intersect / column_union)
}
isTRUE(jaccard(set1, set2) == column_jaccard(m[, 1], m[, 2]))

## ----toy-example-minhash-v1----------------------------------------------
# for our toy example we will pick N = 4
N <- 4
sm <- matrix(data = NA_integer_, nrow = N, ncol = ncol(m))
perms <- matrix(data = NA_integer_, nrow = nrow(m), ncol = N)
# calculate indexes for non-zero entries for each column
non_zero_row_indexes <- apply(m, MARGIN = 2, FUN = function(x) which (x != 0) )
for (i in 1 : N) {
  # calculate permutations
  perm <- sample(nrow(m))
  perms[, i] <- perm
  # fill row of signature matrix
  for (j in 1:ncol(m))
    sm[i, j] <-  min(perm[non_zero_row_indexes[[j]]])
}
print(sm)

## ----toy-example-signatures-sim------------------------------------------
jaccard_signatures <-  function(c1, c2) {
  column_intersect <- sum(c1 == c2)
  column_union <- length(c1)
  return(column_intersect / column_union)
}
print(jaccard_signatures(sm[, 1], sm[, 2]))
print(jaccard_signatures(sm[, 1], sm[, 3]))

