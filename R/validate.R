# FIXME
# would be nice to move this into RCpp
#' @export
#' @name validate_candidate_pairs
#' @title Validate candidate pairs - remove false positive candidates
#' @param candidate_indices - \link{data.table} (or \link{data.frame}) containing columns
#' \code{index1}, \code{index2} - indices of documents in candidate pair
#' @param signature_matrix input signature matrix - \code{\link{integer}} \code{\link{matrix}}
#' @param similarity similarity threshold. Pairs with signatures similarity >= threshold
#' will become near-neighbors.
#' Pairs with signatures similarity < threshold will treated as false positives and filtered out.
#' @return \link{data.table} (or \link{data.frame}) containing columns
#' \code{index1}, \code{index2} - indices of similar documents without false positives
validate_candidate_pairs <- function(candidate_indices, signature_matrix, similarity) {
  validation_function <- get_validation_function(signature_matrix)
  ind <- mapply(function(col1, col2, m) validation_function(m[ , col1 ], m[ , col2]) >= similarity,
                candidate_indices[['index1']],
                candidate_indices[['index2']],
         MoreArgs = list(m = signature_matrix),
         SIMPLIFY = T,
         USE.NAMES = F)
  candidate_indices[which(ind)]
}

get_validation_function <- function(signature_matrix) {
  UseMethod("get_validation_function")
}

get_validation_function.LSHR_Sketch <- function(signature_matrix) {
    cosine_signatures
}

get_validation_function.LSHR_Minhash <- function(signature_matrix) {
  jaccard_atomic
}
