#' Calculate the determinant of a square matrix.
#'
#' This function takes a square matrix as input and computes its determinant.
#' @param mat A square matrix.
#' @return The determinant of the matrix.
#' @examples
#' mat <- matrix(c(1, 2, 3, 4), nrow = 2)
#' matrix_determinant(mat)
matrix_determinant <- function(mat) {
  if (is.matrix(mat) && nrow(mat) == ncol(mat)) {
    det_mat <- det(mat)
    return(det_mat)
  } else {
    stop("Input matrix must be square.")
  }
}
