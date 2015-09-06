#' @name jaccard_atomic
#' @title jaccard similarity between two vectors.
#' @param x - \link{vector}
#' @param y - \link{vector}
#' @return \link{numeric} jaccard similarity coefficient
#'@export
jaccard_atomic <- function(x, y) {
  intrsct <- length(intersect(x, y))
  intrsct / (length(x) + length(y) - intrsct)
}

#' @name cosine_atomic
#' @title cosine similarity between two vectors.
#' @param x - \link{vector}
#' @param y - \link{vector}
#' @return \link{numeric} cosine similarity coefficient
#'@export
cosine_atomic <- function(x, y) {
  as.numeric(x %*% y / sqrt( sum(x^2) * sum(y^2) ))
}

#' @name cosine_signatures
#' @title similarity between two signatures
#' @param x - \link{vector}
#' @param y - \link{vector}
#' @return \link{numeric} cosine similarity coefficient
cosine_signatures <- function(x, y) {
  cos(pi * (1 -sum(x == y) / length(x)))
}

#' @name jaccard_signatures
#' @title similarity between two signatures
#' @param x - \link{vector}
#' @param y - \link{vector}
#' @return \link{numeric} cosine similarity coefficient
jaccard_signatures <- function(x, y) {
  sum(x == y) / length(x)
}


#' @name cosine
#' @title Fast cosine similarity calculation.
#'
#' @param m - sparse \code{\link{dgCMatrix}} or dense input matrix.
#' \code{cosine} calculates all pair similarities between rows of this matrix.
#' @examples
#' i <- c(1,3:8, 1:6); j <- c(2,9,6:10,6:1); x <- c(6 * (1:6), 7 * (1:7))
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

#'@export
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
