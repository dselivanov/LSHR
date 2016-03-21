sketching <- function(dtm, hash_matrix) {
  sketches <- tcrossprod(hash_matrix, dtm)
#   when dtm was sparse
  if (class(sketches) != 'matrix')
    sketches <- as(sketches, 'matrix')
  sketches <- sign(sketches)
  storage.mode(sketches) <- 'integer'
  i0 <- sketches == 0L
  sketches[i0] <- sample(c(1L, -1L), size = sum(i0), replace = T)
  class(sketches) <- 'LSHR_Sketch'
  sketches
}



