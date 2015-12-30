#' @export
get_neighbors <- function(signature_matrix, x, index) {
  UseMethod("get_neighbors")
}
# get_neighbors.LSHR_Sketch <- function(signature_matrix, x, index) {
#   sketching(x, attr(x = signature_matrix, 'hash_matrix'))
# }
#' @export
get_neighbors.LSHR_Minhash <- function(signature_matrix, x, index) {
  sgntr <- minhashing(x, attr(x = signature_matrix, 'hash_matrix'))
  bands_number <- index[, uniqueN(band_index)]
  ind <- create_index(sgntr, bands_number = bands_number, verbose = T)
  index[ind, nomatch = 0][ , .N, by = index]
}
