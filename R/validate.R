#' @export
#' @name validate_candidate_pairs
#' @title Validate candidate pairs - calculates actual distances on candidate pairs
#' @param m - input matrix
#' @param i1 first candidate indices
#' @param i2 second candidate indices
#' @similarity_measure similarity_measure to check. On of \code{"cosine"} or \code{"jaccard"}.
#' \code{index1}, \code{index2} - indices of documents in candidate pair

validate_candidate_pairs <- function(m, i1, i2, similarity_measure = c('cosine', 'jaccard')) {
  similarity_measure <- match.arg(similarity_measure)
  switch(similarity_measure,
         jaccard = validate_jaccard(m, i1, i2),
         cosine = validate_cosine(m, i1, i2),
         stop("similarity_measure not suuported (yet)!"))
}

validate_jaccard <- function(m, i1, i2) {
  # ensure we work with binary matrix
  m <- sign(m)
  m1 <- m[i1, , drop = FALSE]
  m2 <- m[i2, , drop = FALSE]
  intersect <- rowSums(m1 * m2 )
  union <- rowSums( sign( m1 + m2) )
  intersect / union
}

validate_cosine <- function(m, i1, i2) {
  # normalize input
  m <-  Diagonal(nrow(m), 1 / sqrt(Matrix::rowSums(m ^ 2))) %*% m
  rowSums(m[i1, , drop = FALSE] * m[i2, , drop = FALSE])
}
