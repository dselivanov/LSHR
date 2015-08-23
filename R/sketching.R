sketching <- function(dtm, hash_matrix) {
  UseMethod("sketching")
}

sketching.Matrix <- function(dtm, hash_matrix) {
  sketches <- t(dtm %*% hash_matrix)
  # sketches - **numeric matrix** (because of Matrix %*% operator behaviour)
  # **sign()** also returns **numeric** values
  # so we convert it back to integer and then wrap to regular "matrix" class
  signx <- sign(sketches@x) %>% as.integer
  # replace zero-elements of signature marxix whith random {+1L, -1L}
  # (very little channce that dot product of input element
  # and random hyperplane will equal to zero)
  # generally this should not break logic of LSH
  i0 <- signx == 0L
  signx[i0] <- sample(c(1L, -1L), size = sum(i0), replace = T)
  sketches@x <- signx
  # convert back to dense matrix
  sketches <- as(sketches, 'matrix')
  class(sketches) <- 'LSHR_Sketch'
  setattr(sketches, 'dimnames', NULL)
  # keep hash_matrix for near-neighbor search queries
  setattr(x = sketches, name = 'hash_matrix', value = hash_matrix)
  sketches
}

sketching.list <- function(dtm, hash_matrix) {
  sketching.Matrix(lil_to_ngC(dtm), hash_matrix)
}
