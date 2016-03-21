# vectorized version of minhash algorithm with many hash functions
minhashing <- function(dtm, hash_matrix, ...) {

  if (!inherits(dtm, 'dgCMatrix'))
    dtm <- as(dtm, 'dgCMatrix')

  dtm <- LSHR:::to_lil( t(dtm) )

  minhash_signatures <-
    parallel::mcmapply(
      function(nnz, hm) {
        matrixStats::rowMins(hm[, nnz, drop = FALSE])
      },
      dtm,
      MoreArgs = list(hm = hash_matrix), ...)

  minhash_signatures
}

#' @name to_lil
#' @title Converts 'dgCMatrix' to 'lil' format
#' @description Converts 'dgCMatrix' (or coercible to 'dgCMatrix') to 'lil'
#' format. This function is specially useful when you want to do LSH for
#' "jaccard" similarity (which current minhashing algorithm work with
#' List-of-Lists input)
#' @param dtm Document-Term matrix
to_lil <- function(dtm) {
  Map(f = function(i1,i2, ind) ind[i1:i2],
      dtm@p[-length(dtm@p)] + 1L,
      dtm@p[-1L],
      MoreArgs = list(ind = dtm@i + 1L),
      USE.NAMES = F)
}
