#' @export
#' @name get_hash_matrix
#' @title Calculating hashing matrix for given similarity measure.
#' @param terms_size number of
#' @param hashfun_number - number of projections to use for approximation. Involves
#' tradeoff: larger is better, but computattionally more expensive and requires more RAM
#' @param measure Similarity measure
#' @param cores number of CPU cores to use for hashing. (related only for "jaccard" measure)
#' @description For "jaccard" we generate construct hashing matrix
#' which simulate to permutation of rows. For "cosine" we generate random
#' hyperplanes.
get_hash_matrix <- function(dtm, hashfun_number, measure, cores) {
  terms_size <- get_terms_size(dtm)
  hash_matrix <-
    if(measure == 'jaccard') {
      # calculate hashes for each hash function and each row number
      get_minhash_matrix(unique_shingles_length = terms_size,
                         hashfun_number = hashfun_number,
                         cores = cores)
    } else if(measure == 'cosine') {
      # random_hyperpalnes
      matrix(sample(x = c (1L, -1L),
                    size = terms_size * hashfun_number,
                    replace = T),
             ncol = hashfun_number)
    } else
      stop(paste(measure, "not supported"))
  hash_matrix
}

#' @export
#' @name get_signature_matrix
#' @title Calculating signature matrix for given similarity measure.
#'
#' @param dtm_lil - sparse document-term input matrix in list of lists form: \code{\link{list}} of \code{\link{vector}}s
#' length(dtm_lil) = number of documents.
#' It also may contain ncol attribute - number of terms.
#' @param hashfun_number number of hash functions to use in construction of signature matrix.
#' @param measure similarity measure we want to preserve in signature matrix: \code{\link{character}}.
#' Possible values - \code{c('jaccard', 'cosine')}
#' @param cores number of CPU threads to use while calculating hashes.
#'Currently related only to "jaccard" measure. Uses OpenMP under the hood.
#'
#' @return The signature matrix - \code{\link{integer}} \code{\link{matrix}} with dimension \code{hashfun_number * length(dtm_lil)}
#'
#' @examples
#' sets <- lapply(1:10, function(x) sample(letters, sample(5:15)))
#' dtm_lil <- create_dtm(sets)
#' sm <- get_signature_matrix(dtm_lil, hashfun_number = 10, measure = 'jaccard', cores = 4)
get_signature_matrix <- function (dtm, hashfun_number, measure = 'jaccard', cores = parallel::detectCores()) {
  hash_matrix <- get_hash_matrix(dtm, hashfun_number, measure, cores)

  if(measure == 'jaccard') {
    return( minhashing(dtm, hash_matrix) )
  }
  if(measure == 'cosine') {
    return( sketching(dtm, hash_matrix) )
  }
  stop(paste(measure , 'is not supported'))
}
