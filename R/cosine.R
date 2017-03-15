#' @export
get_similar_pairs_cosine <- function(X, bands_number, rows_per_band, seed = 1L, verbose = FALSE,
                                     mc.cores = 1, n_band_join = bands_number, ...) {
  lsh_start = Sys.time()
  PACK_BITS = 32L
  stopifnot(rows_per_band <= 32L)

  if(inherits(X, "sparseMatrix"))
    if(!inherits(X, "dgRMatrix")) {
      message(sprintf("converting input sparse matrix to dgRMatrix"))
      X = as(X, "RsparseMatrix")
    }

  set.seed(seed)
  N = nrow(X)

  pad_bits_matrix = NULL
  if(rows_per_band < PACK_BITS)
    pad_bits_matrix = matrix(0L, ncol = PACK_BITS - rows_per_band, nrow = N)
  # hashfun_number = bands_number * rows_per_band
  # allocate memory for result
  n_chunks = ceiling(bands_number / n_band_join)
  result = NULL
  # suppressWarnings for case when "data length is not a multiple of split variable"
  suppressWarnings({batch_indices = split(1:bands_number, rep(seq_len(n_chunks), each = n_band_join))})
  for(bi in batch_indices) {

    # sketches = parallel::mclapply(seq_len(bands_number), function(i) {
    sketches = parallel::mclapply(bi, function(i) {
      start = Sys.time()

      # project input matrix to several random hyperplanes
      # at the moment 2 different algorithms fir sparse and dense matrices
      # for sparse matrices notably more optimized - C++ code and bit arifmetics
      x =
        if(inherits(X, "sparseMatrix")) {
          cores = ifelse(mc.cores > 1, 1, 0)
          project_spmat(X, rows_per_band, hash_fun_id_offest = seed + i * PACK_BITS, cores )
        } else
        {
          # FIXME - will be faster to generate projections "on the fly" instead of generating `sample` and then multilply
          hm = matrix(runif(ncol(X) * rows_per_band, -2**16, 2**16), nrow = rows_per_band)
          x = tcrossprod(X, hm)
          if(class(x) != 'matrix') x = as.matrix(x)
          x = sign_bit(x)
          if(!is.null(pad_bits_matrix)) x = cbind(x, pad_bits_matrix)
          packBits(t(x), "integer")
        }
      sketch_time = difftime(Sys.time(), start, units = 'secs')
      #------------------------------------------------------
      start = Sys.time()
      dt = data.frame(hash_val = x, id1 = seq_len(N))
      setDT(dt)
      dt[, id2 := id1]
      # join with itself to generate all candidates pairs
      dt = dt[dt, on = .(hash_val = hash_val, id1 > id2), nomatch = 0, allow.cartesian = T]
      pair_self_join_time = difftime(Sys.time(), start, units = 'secs')
      # caclulate how many times each pair became candidate - local reduce
      start = Sys.time()
      dt = dt[, .N, keyby = .(id1, id2)]
      pair_reduce_time = difftime(Sys.time(), start, units = 'secs')
      if (verbose) message( sprintf( "%s band %02d sketch_time %.3f; self_join: %.3f sec; local_reduce: %.3f sec",
                                     Sys.time(), i, sketch_time, pair_self_join_time, pair_reduce_time ) )
      #------------------------------------------------------
      dt
    }, mc.cores = mc.cores, ...)
    start = Sys.time()
    sketches = rbindlist(sketches);gc()
    sketches = sketches[, .(N = sum(N)), keyby = .(id1, id2)];gc()
    result = rbindlist(list(result, sketches));
    rm(sketches);gc()
    result = result[, .(N = sum(N)), keyby = .(id1, id2)];gc()
    if (verbose) message( sprintf( "%s GLOBAL REDUCE after band # %d: %.3f sec",
                                   Sys.time(), tail(bi, 1),
                                   difftime(Sys.time(), start, units = 'secs')))
  }
  if (verbose) message( sprintf( "%s TOTAL TIME SPENT %.3f sec", Sys.time(), difftime(Sys.time(), lsh_start, units = 'secs')))
  result
}
