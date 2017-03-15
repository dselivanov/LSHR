get_similar_pairs_jaccard = function(X, bands_number, rows_per_band, seed = 1L, verbose = TRUE) {

  hash_matrix = get_minhash_matrix(unique_shingles_length = ncol(X),
                                   hashfun_number = rows_per_band * bands_number,
                                   seed = seed)
  signature_matrix = minhashing(X, hash_matrix); rm(hash_matrix)

  sm_nrow = nrow(signature_matrix)
  sm_ncol = ncol(signature_matrix)

  start = Sys.time()
  hashed_signatures = hash_signatures(m = signature_matrix, bands_number, rows_per_band = sm_nrow / bands_number)
  if (verbose) message( sprintf( "hashing in %.3f sec", difftime(Sys.time(), start, units = 'secs') ) )

  dim(hashed_signatures) = NULL
  start = Sys.time()
  dt = data.table(hash_val = hashed_signatures,
                  band_id  = rep(seq_len(bands_number), each = sm_ncol),
                  id1      = rep(seq_len(sm_ncol),times = bands_number ))
  dt[, id2 := id1]

  start = Sys.time()
  # join with itself to receive candidates pairs
  dt = dt[dt, on = .(hash_val = hash_val, band_id = band_id, id1 < id2), nomatch = 0, allow.cartesian = T]
  if (verbose) print( sprintf( "generating pairs %.3f sec", difftime(Sys.time(), start, units = 'secs') ) )

  start = Sys.time()
  # caclulate how many times each pair became candidate
  dt = dt[, .N, keyby = .(id1, id2)]
  if (verbose) print( sprintf( "finding unique pairs %.3f sec", difftime(Sys.time(), start, units = 'secs') ) )

  dt
}

# vectorized version of minhash algorithm with many hash functions
minhashing <- function(dtm, hash_matrix, ...) {

  if (!inherits(dtm, 'dgCMatrix'))
    dtm <- as(dtm, 'dgCMatrix')

  dtm <- to_lil( t(dtm) )

  minhash_signatures <-
    parallel::mcmapply(
      function(nnz, hm) {
        matrixStats::rowMins(hm[, nnz, drop = FALSE])
      },
      dtm,
      MoreArgs = list(hm = hash_matrix), ...)

  minhash_signatures
}

# @name to_lil
# @title Converts 'dgCMatrix' to 'lil' format
# @description Converts 'dgCMatrix' (or coercible to 'dgCMatrix') to 'lil'
# format. This function is specially useful when you want to do LSH for
# "jaccard" similarity (which current minhashing algorithm work with
# List-of-Lists input)
# @param dtm Document-Term matrix
to_lil <- function(dtm) {
  Map(f = function(i1,i2, ind) ind[i1:i2],
      dtm@p[-length(dtm@p)] + 1L,
      dtm@p[-1L],
      MoreArgs = list(ind = dtm@i + 1L),
      USE.NAMES = F)
}
