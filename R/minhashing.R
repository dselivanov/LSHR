# vectorized version of minhash algorithm with many hash functions

minhashing <- function(dtm, hash_matrix) {
  UseMethod("minhashing")
}

minhashing.list <- function(dtm_lil, hash_matrix) {
  MAX_INT = .Machine$integer.max
  hashfun_number = ncol(hash_matrix)
  p <- length(dtm_lil)
  minhash_signatures <- matrix(data = rep(MAX_INT, hashfun_number * p), nrow = hashfun_number, ncol = p)
  for (clmn in seq_along(dtm_lil)) {
    mat_non_zero_rows <- dtm_lil[[clmn]]
    mat <- hash_matrix[mat_non_zero_rows, , drop = FALSE]
    minhash_signatures[, clmn] <- pmin.int(minhash_signatures[, clmn],  matrixStats::colMins(mat))
  }
  class(minhash_signatures) <- 'LSHR_Minhash'
  # keep hash_matrix for near-neighbor search queries
  setattr(x = minhash_signatures, name = 'hash_matrix', value = hash_matrix)
  minhash_signatures
}

minhashing.Matrix <- function(dtm, hash_matrix) {
  minhashing.list(ngC_to_lil(dtm), hash_matrix)
}
