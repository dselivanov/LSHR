#' @export
#' @name get_similar_pairs
#' @title Calculating candidate pairs using locality sensitive hashing.
#'
#' @param signature_matrix input signature matrix - \code{\link{integer}} \code{\link{matrix}}
#' @param bands_number number of bands for LSH algorithm.
#' @param verbose - \link{logical} print lsh process information.
#' (such as expected false positive rate, false negative rate,timings, etc.)
#' @return pairs of candidates with similarity => \code{similarity} -
#' \code{\link{data.table}} with 3 colums: index1, index2, N -
#' index of first candidate, index of second candidate,
#' and number of buckets where they share same value. The latter provided only for information.
#' (Intutition is following: the bigger N - the stronger similarity)
#'
#' @examples
#' sets = lapply(1:10, function(x) sample(letters, sample(5:15)))
#' # add set similar to first set to the end of list
#' sets = c(sets, list(c(sets[[1]], sample(letters, 5))))
#' sm = get_signature_matrix(sets, 12)
#' get_similar_pairs(sm, 6, 0.9)
get_similar_pairs = function(signature_matrix, bands_number, verbose = TRUE) {
  sm_nrow = nrow(signature_matrix)
  sm_ncol = ncol(signature_matrix)

  if ( sm_nrow %% bands_number != 0)
    stop("number of bands should be divisor of number of rows of signature matrix: 0 == nrow(signature_matrix) %% bands_number")

  # create hash tables - one per bucket
  # lsh_index = sorted hash tables
  start = Sys.time()
  hashed_signatures = hash_signatures(m = signature_matrix, bands_number, rows_per_band = sm_nrow / bands_number)
  if (verbose) print( sprintf( "hashing in %.3f sec", difftime(Sys.time(), start, units = 'secs') ) )

  # make array - unlist by column
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

