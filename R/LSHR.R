#' LSHR: Locality Sensitive haching in R
#'
#' @docType package
#' @name LSHR
#' @useDynLib LSHR
#' @import data.table
#' @import magrittr
#' @import methods
#' @import Matrix
#' @importFrom bit64 integer64
#' @import futile.logger
#' @importFrom Rcpp evalCpp
#' @importFrom ggplot2 ggplot geom_line aes scale_color_discrete
NULL

#' @export
#' @name get_similar_pairs
#' @title Calculating candidate pairs using locality sensitive hashing.
#' @description For a given matrix function generate indices of similar rows.
#' @param X input matrix - sparse or dense
#' @param bands_number number of bands for LSH algorithm - tradeoff between precision and
#' number of false positive candidates. See \link{get_s_curve} for details.
#' @param rows_per_band number of rows in each band for LSH algorithm - tradeoff between precision and
#' number of false positive candidates. See \link{get_s_curve} for details.
#' For "cosine" distance due to performance reasons (bit arifmetics) only values less than 32 are supported.
#' @param distance on of "cosine" or "jaccard" - how to measure distance between rows of input matrix
#' @param seed random seed for reproducibility
#' @param verbose \code{logical} print lsh process information.
#' (such as expected false positive rate, false negative rate,timings, etc.)
#' @return pairs of candidates with similarity => \code{similarity} -
#' \code{\link{data.table}} with 3 colums: id1, id2, N -
#' index of first candidate, index of second candidate,
#' and number of buckets where they share same value. The latter provided only for information.
#' (Intutition is following: the bigger N - the stronger similarity)
#'
#' @examples
#' \dontrun{
#' library(text2vec)
#' library(LSHR)
#' library(Matrix)
#' data("movie_review")
#' it <- itoken(movie_review$review, preprocess_function = tolower,
#' tokenizer = word_tokenizer)
#' dtm <- create_dtm(it, hash_vectorizer())
#' dtm = as(dtm, "RsparseMatrix")
#' pairs = get_similar_pairs(dtm, bands_number = 4, rows_per_band = 32,
#' distance = 'cosine', verbose = TRUE)
#' pairs[order(-N)]
#' }

get_similar_pairs = function(X, bands_number, rows_per_band, distance = c("cosine", "jaccard"), seed = 1L,
                             verbose = FALSE, mc.cores = 1, ...) {
  distance = match.arg(distance)
  switch(distance,
         jaccard = get_similar_pairs_jaccard(X, bands_number, rows_per_band, seed, verbose, ...),
         cosine =  get_similar_pairs_cosine (X, bands_number, rows_per_band, seed, verbose, mc.cores = mc.cores, ...)
  )
}


globalVariables(c("n_bands", "probability_become_candidate", "similarity", "n_rows_per_band", "band_id", "hash_val", "id1", "id2", "."))
