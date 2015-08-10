# for debug
#'@export
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

# for debug
#'@export
jaccard_atomic <- function(x, y) {
  intrsct <- length(intersect(x, y))
  intrsct / (length(x) + length(y) - intrsct)
}

# for debug
#'@export
cosine_atomic <- function(x, y) {
  as.numeric(x %*% y / sqrt( sum(x^2) * sum(y^2) ))
}

#' @name cosine
#' @title Fast cosine similarity calculation.
#'
#' @param m - sparse \code{\link{dgCMatrix}} or dense input matrix.
#' \code{cosine} calculates all pair similarities between rows of this matrix.
#' @examples
#' i <- c(1,3:8, 1:6); j <- c(2,9,6:10,6:1); x <- c(6* (1:6), 7 * (1:7))
#' m <- sparseMatrix(i, j, x = x)
#' cosine(m)
#'@export
cosine <- function(m) {
  # http://stats.stackexchange.com/a/81877/37081
  if(inherits(m, 'Matrix'))
    m =  as(m, 'dgCMatrix')
  else if(!inherits(m, 'matrix'))
    stop("m should be value of matrix or Matrix classes")
  # normalize each row so it's vector distance is 1
  row_norms <- sqrt(Matrix::rowSums(m^2))
  row_norms <- t(crossprod(sign(m), Diagonal(x=row_norms)))
  row_norms@x <- 1/row_norms@x
  m_norm <- m * row_norms
  tcrossprod(m_norm)
}

#' @name jaccard
#' @title Fast jaccard similarity calculation.
#'
#' @param m - sparse \code{\link{dgCMatrix}} (or object that can be coerced to)
#' or dense input matrix.
#' \code{jaccard} calculates all pair similarities between rows of this matrix.
#' @examples
#' i <- c(1,3:8, 1:6); j <- c(2,9,6:10,6:1); x <- c(6* (1:6), 7 * (1:7))
#' m <- sparseMatrix(i, j, x = x)
#' jaccard(m)
#'@export
jaccard <- function(m) {
  # http://stats.stackexchange.com/a/89947/37081
  if(inherits(m, 'Matrix'))
    m =  as(m, 'dgCMatrix')
  else if(!inherits(m, 'matrix'))
    stop("m should be value of matrix or Matrix classes")
  ## common values:
  A <- tcrossprod(m)
  ## indexes for non-zero common values
  im <- which(A > 0, arr.ind=TRUE, useNames = F)
  ## counts for each row
  b <- rowSums(m)

  ## only non-zero values of common
  Aim <- A[im]

  ## Jacard formula: #common / (#i + #j - #common)
  sparseMatrix(
    i = im[,1],
    j = im[,2],
    x = Aim / (b[im[,1]] + b[im[,2]] - Aim),
    dims = dim(A)
  )
}
