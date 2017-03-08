get_similar_pairs_cosine <- function(X, bands_number, rows_per_band, seed = 1L, verbose = FALSE,
                                     mc.cores = 1, ...) {
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
  hashfun_number = bands_number * rows_per_band
  # allocate memory for result
  # sketches = vector("list", length(bands_number))
  # for(i in seq_len(bands_number)) {
  sketches = parallel::mclapply(seq_len(bands_number), function(i) {
    start = Sys.time()

    # project input matrix to several random hyperplanes
    # at the moment 2 different algorithms fir sparse and dense matrices
    # for sparse matrices notably more optimized - C++ code and bit arifmetics
    x =
      if(inherits(X, "sparseMatrix")) {
        cores = ifelse(mc.cores > 1, 1, 0)
        project_spmat(X, rows_per_band, seed = seed + i * PACK_BITS, cores )
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
    if (verbose) message( sprintf( "band %02d - generating sketch %.3f sec", i, difftime(Sys.time(), start, units = 'secs') ) )
    #------------------------------------------------------
    start = Sys.time()
    dt = data.frame(hash_val = x, band_id = i, id1 = seq_len(N))
    setDT(dt)
    dt[, id2 := id1]
    # join with itself to receive candidates pairs
    dt = dt[dt, on = .(hash_val = hash_val, band_id = band_id, id1 > id2), nomatch = 0, allow.cartesian = T]
    # caclulate how many times each pair became candidate
    dt = dt[, .N, keyby = .(id1, id2)]
    if (verbose) message( sprintf( "band %02d - generating pairs  %.3f sec", i, difftime(Sys.time(), start, units = 'secs') ) )
    #------------------------------------------------------
    # sketches[[i]] = dt
    # sketches
    dt
  }, mc.cores = mc.cores, ...)
  start = Sys.time()
  sketches = rbindlist(sketches)
  sketches = sketches[, .(N = sum(N)), keyby = .(id1, id2)]
  if (verbose) message( sprintf( "REDUCE all bands stat %.3f sec", difftime(Sys.time(), start, units = 'secs') ) )
  sketches
}
