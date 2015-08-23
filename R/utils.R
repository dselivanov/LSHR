split_vector <- function(x, splits) {
  if(! is.vector(x)) stop("x must be vector or list")
  if (length(x) < splits ) {
    warning("Length of input is too small for splitting for a given number of splits. Assuming no splits.")
    return (list(c(1, length(x))))
  }
  chunkSize = length(x) %/% (splits)
  knots = ceiling(seq.int(from = 1, to = length(x) + 1, length.out = splits  + 1))
  mapply(FUN = function(lower, upper) list(c(lower, upper)), knots[-length(knots)], knots[-1] - 1)
}

get_terms_size <- function(dtm) {
  # recieve sparse matrix in one on Matrix package formats (not in List-of-lists! form)
  if(inherits(dtm, "Matrix")) {
    terms_size <- ncol(dtm)
  } else
    # List-of-lists form
  {
    terms_size <- length(attr(dtm, 'terms'))
    # if we don't know number of columns of lil dtm (it come not from LSHR::create_dtm)
    if(is.null(terms_size))
      terms_size <- unlist(dtm_lil, recursive = T, use.names = F) %>% unique %>% length
  }
  terms_size
}
