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

jaccard <- function(x, y) {
  length(intersect(x, y)) / length(union(x, y))
}
