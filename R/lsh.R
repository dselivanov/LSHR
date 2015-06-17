#' @export
#' @name get_candidate_pairs
#' @title Calculating candidate pairs using locality sensitive hashing.
#'
#' @param signature_matrix input signature matrix - \code{\link{integer}} \code{\link{matrix}}
#' @param bands_number number of bands for LSH algorithm.
#' @param similarity target value of jacard similarity we are looking for.
#' @param verbose - \link{logical} print useful algorithm information.
#' @return pairs of candidates with similarity => \code{similarity} -
#' \code{\link{data.table}} with 3 colums: index1, index2, N -
#' index of first candidate, index of second candidate,
#' and number of buckets where they share same value. The latter provided
#' only for information.
#' (Intutition is following: the bigger N - the bigger similarity)
#'
#'
#' @examples
#' sets <- lapply(1:10, function(x) sample(letters, sample(5:15)))
#' # add set similar to first set to the end of list
#' sets <- c(sets, list(c(sets[[1]], sample(letters, 5))))
#' sm <- get_signature_matrix(sets, 12, cores = 4)
#' get_candidate_pairs(sm, 6, 0.9)
get_candidate_pairs <- function(signature_matrix, bands_number, similarity, verbose = TRUE) {
  # signature_matrix <- get_signature_matrix(sets, hashfun_number, cores = cores)
  sm_nrow <- nrow(signature_matrix)
  if( sm_nrow %% bands_number != 0)
    stop("number of bands should be divisor of number of rows of signature matrix: 0 == nrow(signature_matrix) %% bands_number")
  rows_per_band <- sm_nrow / bands_number
  if(verbose) {
    prob_become_candidate <- 1 - (1 - similarity ^ rows_per_band) ^ bands_number
    print(paste('Looking for sets with similarity',
                round(similarity, 2)))
    print(paste('Probablity of becoming candidate pair =',
                prob_become_candidate))
  }

  # calculate bands borders for splitting signarure matrix
  splits <- split_vector(x = 1:sm_nrow, splits = bands_number)
  bands_hashes <- Map(hash_band, splits,
                 MoreArgs = list(signature_matrix = signature_matrix ))

  candidate_pairs <- detect_candidate_pairs(bands_hashes)
  candidate_pairs
}

detect_candidate_pairs <- function(band_hashes) {
  bands_number <- length(band_hashes)
  # all bands_hashes have equal row lengths
  rows_per_band <- length(band_hashes[[1]])
  # construct table for
  dt <- data.table(index = rep(1:rows_per_band, bands_number),
                   hash_value = unlist(band_hashes),
                   band_index = rep(1:bands_number, each = rows_per_band),
                   key = c('band_index', 'hash_value'))

  # look for candidates that have same hash value in a given band.
  dt = dt[ , list(index, .N), keyby = list(band_index, hash_value) ][N > 1, .(band_index, hash_value, index)]
  # cross join to find candidate pair indices
  dt = dt[dt, list(index1 = index, index2 = i.index), allow.cartesian = TRUE][index1 < index2]
  # combine duplicated candidate pairs and get number of bands where band hashes are identical
  dt[, .N, keyby = c('index1', 'index2')]#[N > 1]
}

hash_band <- function(row_index_bounds, signature_matrix) {
  row_indices <- row_index_bounds[[1]]:row_index_bounds[[2]]
  # Using simple sum() as hash of signature chunk in each band.
  # We can (and should) construct better hash function for hashing integer sequences
  colSums(signature_matrix[row_indices, , drop = FALSE])
}
