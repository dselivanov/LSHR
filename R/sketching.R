sketching <- function(tdm_lil, hash_matrix) {
  #
  tdm_csr <- lil_to_csr(tdm_lil)
  sketches <-  t(tdm_csr %*% hash_matrix)
  # sketches - **numeric matrix** (because of Matrix %*% operator behaviour)
  # **sign()** also returns **numeric** values
  # so we convert it back to integer and then wrap to regular "matrix" class
  signx <- sign(sketches@x) %>% as.integer
  i0 <- signx == 0L
  signx[i0] <- sample(c(1L, -1L), size = sum(i0), replace = T)
  sketches@x <- signx
  sketches <- as(sketches, 'matrix')
  setattr(sketches, 'dimnames', NULL)
  # setattr(x = sketches, name = 'lshr_family', value = 'sketch')
  class(sketches) <- 'LSHR_Sketch'
  setattr(x = sketches, name = 'hash_matrix', value = hash_matrix)
  sketches
}
