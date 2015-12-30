# vectorized version of minhash algorithm with many hash functions
minhashing <- function(dtm, hash_matrix) {

  if (!inherits(dtm, 'dgCMatrix'))
    dtm <- as(dtm, 'dgCMatrix')

  ndoc = nrow(dtm)

  dtm <- to_lil( t(dtm) )

  MAX_INT = .Machine$integer.max
  hashfun_number = ncol(hash_matrix)

  minhash_signatures <- matrix(data = rep(MAX_INT, hashfun_number * ndoc), nrow = hashfun_number, ncol = ndoc)
  for (clmn in seq_along(dtm)) {
    mat_non_zero_rows <- dtm[[clmn]]
    mat <- hash_matrix[mat_non_zero_rows, , drop = FALSE]
    minhash_signatures[, clmn] <- pmin.int(minhash_signatures[, clmn],  matrixStats::colMins(mat))
  }
  class(minhash_signatures) <- 'LSHR_Minhash'
  # keep hash_matrix for near-neighbor search queries
  setattr(x = minhash_signatures, name = 'hash_matrix', value = hash_matrix)
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
