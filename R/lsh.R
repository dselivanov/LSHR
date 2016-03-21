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
#' sets <- lapply(1:10, function(x) sample(letters, sample(5:15)))
#' # add set similar to first set to the end of list
#' sets <- c(sets, list(c(sets[[1]], sample(letters, 5))))
#' sm <- get_signature_matrix(sets, 12)
#' get_similar_pairs(sm, 6, 0.9)
get_similar_pairs <- function(signature_matrix, bands_number, verbose = TRUE) {
  sm_nrow <- nrow(signature_matrix)
  sm_ncol <- ncol(signature_matrix)

  if ( sm_nrow %% bands_number != 0)
    stop("number of bands should be divisor of number of rows of signature matrix: 0 == nrow(signature_matrix) %% bands_number")

  # create hash tables - one per bucket
  # lsh_index = sorted hash tables
  start <- Sys.time()
  hashed_signatures <- hash_signatures(m = signature_matrix, bands_number, rows_per_band = sm_nrow / bands_number)
  if (verbose)
    print( paste( "hashing takes", round(difftime(Sys.time(), start, units = 'secs'), 3), 'sec' ) )

  # make array - unlist by column
  dim(hashed_signatures) <- NULL
  start <- Sys.time()
  dt = data.table(hash_val = hashed_signatures,
                  band_id = rep(1:bands_number, each = sm_ncol),
                  id = rep(1:sm_ncol, times = bands_number ),
                  key = c('hash_val',  'band_id'))
  if (verbose)
    print( paste( "indexing", round(difftime(Sys.time(), start, units = 'secs'), 3), 'sec' ) )

  start <- Sys.time()
  comb = dt[, list(candidate_ids = list(id), .N), by = .(hash_val, band_id)][N > 1, .(candidate_ids, band_id)]
  if (verbose)
    print( paste( "combining", round(difftime(Sys.time(), start, units = 'secs'), 3), 'sec' ) )

  start <- Sys.time()
  res = pairs(comb$candidate_ids)
  if (verbose)
    print( paste( "generating pairs ", round(difftime(Sys.time(), start, units = 'secs'), 3), 'sec' ) )

  setDT(res)
}

