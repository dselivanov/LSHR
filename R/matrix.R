#' @export
#' @name create_tdm
#' @title Construct Term-Document matrix (in "List of Lists" form) from input list of elements.
#'
#' @param lst input list of values in a form of \code{\link{list}} of \code{\link{vector}}s
create_tdm <- function(lst) {
  # make sparse binary term-document matrix (in "List of Lists" form) :
  # rows = unique elements of input list, cols = ids of lists which cantain that elements
  # values = [TRUE,FALSE] - whether given set contains given element of set
  # we store matrix as list of arrays. So we keep only TRUE values.
  # get_tdm_character returns 2 things:
  # 1) total number of unique elements in all sets
  # 2) sparse term-document matrix represented by list.
  get_tdm_character(lst)
}

# http://stackoverflow.com/a/4946174/1069256
#'# @export
lil_to_csr <- function(m_lil) {
  n_ids <- sapply(m_lil, length)
  vals <- unlist(m_lil, recursive = T, use.names = F)
  sparseMatrix(i = rep(seq_along(n_ids), n_ids), j = vals)
}

csr_to_lil <- function(tdm) {
  Map(FUN = function(i1,i2, ind) ind[i1 : i2],
         tdm@p[-length(tdm@p)] + 1L,
         tdm@p[-1L], MoreArgs = list(ind = tdm@i + 1L),
         USE.NAMES = F)
}
