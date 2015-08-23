#' @export
#' @name create_dtm
#' @title Construct binary Document-Term matrix
#' from input list of elements.
#' @param lst input list of values in a form of
#' \code{\link{list}} of \code{\link{vector}}s
#' @param format desired format of result.
#' \code{lil} - List of Lists format
#' \code{ngC} - \code{\link{ngCMatrix-class}}
#' @description This function is specially useful when creating
#' Document-Term matrix from input list of sets for "minhashing"
#' @examples
#' generate artificial elements of sets
#' elems <- sapply(1:100, function(z)
#' paste(sample(letters, sample(3:7), replace = T), collapse=''))
#' generate artificial sets
#' sets <-  lapply(1:1e3, function(z) sample(elems, sample(20:100)))
#' dtm_lil <- create_dtm(sets, format = "lil")
create_dtm <- function(lst, format = c("lil", "ngC") ) {
  format <- match.arg(format)
  # rows = unique elements of input list, cols = ids of lists which cantain that elements
  # values = [TRUE,FALSE] - whether given set contains given element of set
  # we store matrix as list of arrays. So we keep only TRUE values.
  # get_dtm_character returns 2 things:
  # 1) sparse document-term matrix represented by list.
  # 2) unique terms in attribute "terms"
  dtm_lil <- get_dtm_character(lst)
  # class(dtm_lil) <- 'LSHR_dtm_lil'
  switch (format,
    lil = dtm_lil,
    ngC = {
        lil_to_ngC(dtm_lil)
        # res <- lil_to_ngC(dtm_lil)
        # class(res) <- c('LSHR_dtm_lil' , class(res))
        # res
      },
    stop( paste(format, "format is not supported"))
  )
}

# http://stackoverflow.com/a/4946174/1069256
#' @export
#' @name lil_to_ngC
#' @title Convert binary Document-Term matrix from List-of-Lists
#' format to \code{\link{ngCMatrix-class}}
#' #' @description This function is specially useful when you want to do LSH for
#' "cosine" similarity (which current sketching algorithm work with ngCMatrix input)
#' @examples
#' elems <- sapply(1:1000, function(z)
#' paste(sample(letters, sample(3:7), replace = T), collapse=''))
#' # generate artificial sets
#' sets <-  lapply(1:1e5, function(z) sample(elems, sample(20:100)))
#' system.time(dtm_lil <- create_dtm(sets, format = "lil"))
# dtm_ngC <- lil_to_ngC(dtm_lil)
lil_to_ngC <- function(m_lil) {
  n_ids <- sapply(m_lil, length)
  vals <- unlist(m_lil, recursive = T, use.names = F)
  sparseMatrix(i = rep(seq_along(n_ids), n_ids),
               j = vals,
               dimnames = list(documents = NULL, terms = attr(m_lil, "terms")))
}

#' @export
#' @name ngC_to_lil
#' @title Convert binary Document-Term matrix from \code{\link{ngCMatrix-class}}
#' format List-of-Lists
#' @description This function is specially useful when you want to do LSH for
#' "jaccard" similarity (which current minhashing algorithm work with List-of-Lists input)
#' @examples
#' elems <- sapply(1:1000, function(z)
#' paste(sample(letters, sample(3:7), replace = T), collapse=''))
#' # generate artificial sets
#' sets <-  lapply(1:1e5, function(z) sample(elems, sample(20:100)))
#' system.time(dtm_ngC <- create_dtm(sets, format = "ngC"))
# dtm_lil <- ngC_to_lil(dtm_ngC)
ngC_to_lil <- function(dtm) {
  res <-
    Map(FUN = function(i1,i2, ind) ind[i1 : i2],
      dtm@p[-length(dtm@p)] + 1L,
      dtm@p[-1L],
      MoreArgs = list(ind = dtm@i + 1L),
      USE.NAMES = F)
  attr(res, "terms") <- dtm@Dimnames[[2]]
}
