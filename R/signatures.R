#' @export
#' @name get_hash_matrix
#' @title Calculating hashing matrix for given similarity measure.
#' @param hashfun_number - number of projections to use for approximation. Involves
#' tradeoff: larger is better, but computattionally more expensive and requires more RAM
#' @param measure Similarity measure
#' @description For "jaccard" we generate construct hashing matrix
#' which simulate to permutation of rows. For "cosine" we generate random
#' hyperplanes.
get_hash_matrix <- function(dtm, hashfun_number, measure = c('jaccard', 'cosine'), seed = 0L) {
  set.seed(seed)
  vocab_size <- ncol(dtm)
  measure <- match.arg(measure)
  hash_matrix <-
    switch(measure,
      # calculate hashes for each hash function and each row number
      jaccard = get_minhash_matrix(unique_shingles_length = vocab_size,
                                   hashfun_number = hashfun_number, seed = seed),
      # random_hyperpalnes
      cosine =  matrix(sample(x = c(1L, -1L),
                              size = vocab_size * hashfun_number,
                              replace = T),
                       nrow = hashfun_number),
      stop(paste(measure, "not supported")))

  hash_matrix
}

#' @export
#' @name get_signature_matrix
#' @title Calculating signature matrix for given similarity measure.
#'
#' @param dtm_lil - sparse document-term input matrix in list of lists form:
#' \code{\link{list}} of \code{\link{vector}}s
#' length(dtm_lil) = number of documents.
#' It also may contain ncol attribute - number of terms.
#' @param hashfun_number number of hash functions to use in construction of
#' signature matrix.
#' @param measure similarity measure we want to preserve in signature matrix:
#' \code{\link{character}}. Possible values - \code{c('jaccard', 'cosine')}
#' @param ... parameters to \link{mclapply} function, which is used internally.
#' For example you can provide \code{mc.cores = 4} to run minhashing
#' algorithm on 4 cpu cores.
#'
#' @return The signature matrix - \code{\link{integer}} \code{\link{matrix}}
#' with dimension \code{hashfun_number * length(dtm_lil)}
#'
#' @examples
#' sets <- lapply(1:10, function(x) sample(letters, sample(5:15)))
#' dtm_lil <- create_dtm(sets)
#' sm <- get_signature_matrix(dtm_lil, hashfun_number = 10, measure = 'jaccard',
#' mc.cores = 4)
#'
get_signature_matrix <- function(dtm, hashfun_number,
                                 measure = c('jaccard', 'cosine'),
                                 seed = 0L, ...) {
  measure <- match.arg(measure)
  hash_matrix <- get_hash_matrix(dtm, hashfun_number, measure, seed)
  switch(measure,
          jaccard = minhashing(dtm, hash_matrix, ...),
          cosine = sketching(dtm, hash_matrix),
          stop(paste(measure, 'is not supported'))
          )
}
