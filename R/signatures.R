#' @export
get_hash_matrix <- function(tdm_lil, hashfun_number, measure, cores) {
  ncol <- attr(tdm_lil, 'ncol')
  # if we don't know number of columns of tdm
  if(is.null(ncol))
    ncol <- unlist(tdm_lil, recursive = T, use.names = F) %>% unique %>% length

  hash_matrix <-
    if(measure == 'jaccard') {
      # calculate hashes for each hash function and each row number
      get_minhash_matrix(unique_shingles_length = ncol,
                         hashfun_number = hashfun_number,
                         cores = cores)
    } else if(measure == 'cosine') {
      # random_hyperpalnes
      matrix(sample(x = c (1L, -1L),
                    size = ncol * hashfun_number,
                    # size = nrow(tdm_csr) * hashfun_number,
                    replace = T),
             ncol = hashfun_number)
    } else
      stop(paste(measure, "not supported"))
  hash_matrix
}

#' @export
#' @name get_signature_matrix
#' @title Calculating signature matrix using minhashing techniqe.
#'
#' @param tdm_lil - sparse input matrix in list of lists form: \code{\link{list}} of \code{\link{vector}}s
#' It also may contain ncol attribute - number of columns.
#' @param hashfun_number number of hash functions to calculate signature matrix.
#' @param measure measure we want to calculate: \code{\link{character}}. Possible values - \code{c('jaccard', 'cosine')}
#' @param cores number of CPU threads to use while calculating hashes
#'
#' @return The signature matrix - \code{\link{integer}} \code{\link{matrix}} with dimension \code{hashfun_number * length(tdm_lil)}
#'
#' @examples
#' sets <- lapply(1:10, function(x) sample(letters, sample(5:15)))
#' tdm_lil <- create_tdm(sets)
#' sm <- get_signature_matrix(tdm_lil, hashfun_number = 10, measure = 'jaccard', cores = 4)
get_signature_matrix <- function (tdm_lil, hashfun_number, measure = 'jaccard', cores = parallel::detectCores()) {
  hash_matrix <- get_hash_matrix(tdm_lil, hashfun_number, measure, cores)
  if(measure == 'jaccard') {
    return( minhashing(tdm_lil, hash_matrix) )
  }
  if(measure == 'cosine') {
    return( sketching(tdm_lil, hash_matrix) )
  }
  stop(paste(measure , 'is not supported'))
}
