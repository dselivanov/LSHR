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
  if ( sm_nrow %% bands_number != 0)
    stop("number of bands should be divisor of number of rows of signature matrix: 0 == nrow(signature_matrix) %% bands_number")

  # create hash tables - one per bucket
  # lsh_index = sorted hash tables
  lsh_index <- create_index(signature_matrix, bands_number, verbose)
  candidate_pairs <- detect_candidate_pairs(lsh_index, verbose)
  candidate_pairs
}

create_index <- function(signature_matrix, bands_number, verbose = T) {
  sm_nrow <- nrow(signature_matrix)
  sm_ncol <- ncol(signature_matrix)
  if ( sm_nrow %% bands_number != 0)
    stop("number of bands should be divisor of number of rows of signature matrix: 0 == nrow(signature_matrix) %% bands_number")
  # calculate band borders for splitting signarure matrix
  splits <- split_vector(x = seq_len(sm_nrow), splits = bands_number)
  start <- Sys.time()
  bands_hashes <- Map(hash_band, splits,
                      MoreArgs = list(signature_matrix = signature_matrix ))
  end <- Sys.time()
  if (verbose)
    print( paste( "hashing takes", difftime(end, start) ) )
  start <- Sys.time()
  # construct table for
  index <- data.table(index = rep(seq_len(sm_ncol), bands_number),
                      hash_value = unlist(bands_hashes, use.names = F),
                      band_index = rep(seq_len(bands_number), each = sm_ncol),
                      key = c('band_index', 'hash_value'))
  end <- Sys.time()
  if (verbose)
    print( paste( "indexing takes", difftime(end, start) ) )
  index
}

detect_candidate_pairs <- function(lsh_index, verbose = T) {
  # look for candidates that have same hash value in a given band.
  start <- Sys.time()
  dt = lsh_index[ , list(index, .N), keyby = list(band_index, hash_value) ][N > 1, .(band_index, hash_value, index)]
  # cross join to find candidate pair indices
  dt = dt[dt, list(index1 = index, index2 = i.index), allow.cartesian = TRUE][index1 < index2]
  # combine duplicated candidate pairs and get number of bands where band hashes are identical
  res = dt[, .N, keyby = c('index1', 'index2')]
  end <- Sys.time()
  if (verbose)
    print( paste( "detecting candidates takes", difftime(end, start) ) )
  res
}

# function to hash bands.
hash_band <- function(row_index_bounds, signature_matrix) {
  MAX_INT = .Machine$integer.max
  row_indices <- row_index_bounds[[1]]:row_index_bounds[[2]]
  # hash each band -
  # 1) multiply each value by its position
  # 2) take mod MAX_INT to ensure that there is no int overflow.
  # (overflow is very ok for us! but R cast to numeric when detect int overflow)
  # don't know how to allow integer overflow in R. Suggestions are very welcome.
  # 3) hash that value
  temp <- hashfun_1( (signature_matrix[row_indices, , drop = FALSE] * as.numeric(seq_along(row_indices))) %% MAX_INT)
  setattr(temp, 'dim', c(length(row_indices), ncol(signature_matrix) ))
  as.integer(colSums(temp) %% MAX_INT)
  # colSums(temp)
}
