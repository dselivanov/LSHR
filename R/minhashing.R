# vectorized version of minhash algorithm with many hash functions
minhashing <- function(tdm_lil, hash_matrix) {
  MAX_INT = .Machine$integer.max
  hashfun_number = ncol(hash_matrix)
  p <- length(tdm_lil)
  minhash_signatures <- matrix(data = rep(MAX_INT, hashfun_number * p), nrow = hashfun_number, ncol = p)
  for (clmn in seq_along(tdm_lil)) {
    mat_non_zero_rows <- tdm_lil[[clmn]]
    mat <- hash_matrix[mat_non_zero_rows, , drop = FALSE]
    minhash_signatures[, clmn] <- pmin.int(minhash_signatures[, clmn],  matrixStats::colMins(mat))
  }
  class(minhash_signatures) <- 'LSHR_Minhash'
  setattr(x = minhash_signatures, name = 'hash_matrix', value = hash_matrix)
  minhash_signatures
}

