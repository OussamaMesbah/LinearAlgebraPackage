#' Calculate the inverse of a square matrix.
#'
#' This function takes a square matrix as input and computes its inverse.
#' @param mat A square matrix.
#' @return The inverse matrix if it exists; otherwise, an error is returned.
#' @examples
#' mat <- matrix(c(1, 2, 3, 4), nrow = 2)
#' matrix_inverse(mat)
matrix_inverse <- function(mat) {
  if (is.matrix(mat) && nrow(mat) == ncol(mat)) {
    inv_mat <- solve(mat)
    return(inv_mat)
  } else {
    stop("Input matrix must be square.")
  }
}
