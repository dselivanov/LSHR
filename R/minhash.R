minhashing <- function(hash_matrix, TDM, hashfun_number) {
  p <- length(TDM)
  m_sig <- matrix(data = rep(Inf, hashfun_number * p), nrow = hashfun_number, ncol = p)
  for (c in 1:length(TDM)) {
    mat_non_zero_rows <- TDM[[c]]
    mat <- hash_matrix[mat_non_zero_rows, , drop = FALSE]
    m_sig[, c] <- pmin.int(m_sig[, c],  matrixStats::colMins(mat))
  }
  m_sig
}

#' @export
#' @name get_signature_matrix
#' @title Calculating signature matrix using minhashing techniqe.
#'
#' @param sets input sets in a form of \code{\link{list}} of \code{\link{vector}}s
#' @param hashfun_number number of hash functions to calculate signature matrix.
#' @param cores number of CPU threads to use while calculating hashes
#'
#' @return The signature matrix - \code{\link{integer}} \code{\link{matrix}} with dimension \code{hashfun_number * length(sets)}
#'
#' @examples
#' sets <- lapply(1:10, function(x) sample(letters, sample(5:15)))
#' sm <- get_signature_matrix(sets, 10, cores = 4)
get_signature_matrix <- function (sets, hashfun_number, cores = parallel::detectCores()) {
  shingles <- sets %>% unlist %>% unique
  unique_shingles_length <- length(shingles)
  # make sparse term-document matrix : rows = elements of set, cols = set's ids
  # values = [TRUE,FALSE] - whether given set contains given element of set
  # we store matrix as list of arrays. So we keep only TRUE values:
  # each element of list is an arrays which contains row numbers where elements are TRUE
  TDM <- Map(function(set, dict) fastmatch::fmatch(x = set, table = dict),
                  sets,
                  MoreArgs = list(dict = shingles))
  # calculate hashes for each hash function and each row number
  hash_matrix <- get_hash_matrix(unique_shingles_length = unique_shingles_length,
                                 hashfun_number = hashfun_number,
                                 cores = cores)
  minhashing(hash_matrix, TDM, hashfun_number)
}
