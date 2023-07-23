#' Calculate the inverse of a square matrix.
#'
#' This function takes a square matrix as input and computes its inverse.
#' @param mat A square matrix.
#' @return The inverse matrix if it exists; otherwise, an error is returned.
#' @examples
#' mat <- matrix(c(1, 2, 3, 4), nrow = 2)
#' matrix_inverse(mat)
#' @export
matrix_inverse <- function(mat) {
  if (is.matrix(mat) && nrow(mat) == ncol(mat)) {
    inv_mat <- solve(mat)
    return(inv_mat)
  } else {
    stop("Input matrix must be square.")
  }
}

#' Calculate the determinant of a square matrix.
#'
#' This function takes a square matrix as input and computes its determinant.
#' @param mat A square matrix.
#' @return The determinant of the matrix.
#' @examples
#' mat <- matrix(c(1, 2, 3, 4), nrow = 2)
#' matrix_determinant(mat)
#' @export
matrix_determinant <- function(mat) {
  if (is.matrix(mat) && nrow(mat) == ncol(mat)) {
    det_mat <- det(mat)
    return(det_mat)
  } else {
    stop("Input matrix must be square.")
  }
}

#' Calculate the eigenvalues and eigenvectors of a square matrix.
#'
#' This function takes a square matrix as input and computes its eigenvalues and eigenvectors.
#' @param mat A square matrix.
#' @return A list with components 'values' (eigenvalues) and 'vectors' (eigenvectors).
#' @examples
#' mat <- matrix(c(1, 2, 3, 4), nrow = 2)
#' matrix_eigen(mat)
#' @export
matrix_eigen <- function(mat) {
  if (is.matrix(mat) && nrow(mat) == ncol(mat)) {
    eigen_result <- eigen(mat)
    return(eigen_result)
  } else {
    stop("Input matrix must be square.")
  }
}

#' Perform Singular Value Decomposition (SVD) on a matrix.
#'
#' This function takes a matrix as input and performs Singular Value Decomposition (SVD).
#' @param mat A matrix.
#' @return A list with components 'u', 'd', and 'v' representing the SVD of the matrix.
#' @examples
#' mat <- matrix(c(1, 2, 3, 4), nrow = 2)
#' matrix_svd(mat)
#' @export
matrix_svd <- function(mat) {
  svd_result <- svd(mat)
  return(svd_result)
}

#' Multiply two matrices.
#'
#' This function multiplies two matrices and returns the result.
#' @param mat1 First matrix.
#' @param mat2 Second matrix.
#' @return The product of the two matrices.
#' @examples
#' mat1 <- matrix(c(1, 2, 3, 4), nrow = 2)
#' mat2 <- matrix(c(2, 0, 1, 3), nrow = 2)
#' matrix_multiply(mat1, mat2)
#' @export
matrix_multiply <- function(mat1, mat2) {
  if (ncol(mat1) == nrow(mat2)) {
    result <- mat1 %*% mat2
    return(result)
  } else {
    stop("Matrix dimensions are not compatible for multiplication.")
  }
}

#' Calculate the transpose of a matrix.
#'
#' This function calculates the transpose of a matrix.
#' @param mat A matrix.
#' @return The transpose of the input matrix.
#' @examples
#' mat <- matrix(c(1, 2, 3, 4), nrow = 2)
#' matrix_transpose(mat)
#' @export
matrix_transpose <- function(mat) {
  result <- t(mat)
  return(result)
}

#' Solve a linear system of equations.
#'
#' This function takes a coefficient matrix and a vector of constants as input and solves the linear system.
#' @param A Coefficient matrix.
#' @param b Vector of constants.
#' @return The solution vector.
#' @examples
#' A <- matrix(c(1, 2, 3, 4), nrow = 2)
#' b <- c(5, 6)
#' solve_linear_system(A, b)
#' @export
solve_linear_system <- function(A, b) {
  if (is.matrix(A) && is.vector(b) && nrow(A) == length(b)) {
    solution <- solve(A, b)
    return(solution)
  } else {
    stop("Input A must be a matrix and b must be a vector of appropriate length.")
  }
}

#' Calculate the matrix raised to a power.
#'
#' This function takes a square matrix and a power as input and computes the matrix raised to the power.
#' @param mat A square matrix.
#' @param power The power to which the matrix should be raised.
#' @return The matrix raised to the power.
#' @examples
#' mat <- matrix(c(1, 2, 3, 4), nrow = 2)
#' matrix_power(mat, 2)
#' @export
matrix_power <- function(mat, power) {
  if (is.matrix(mat) && nrow(mat) == ncol(mat)) {
    power_mat <- mat %^% power
    return(power_mat)
  } else {
    stop("Input matrix must be square.")
  }
}

#' Calculate the Frobenius norm of a matrix.
#'
#' This function calculates the Frobenius norm of a matrix.
#' @param mat A matrix.
#' @return The Frobenius norm of the matrix.
#' @examples
#' mat <- matrix(c(1, 2, 3, 4), nrow = 2)
#' matrix_frobenius_norm(mat)
#' @export
matrix_frobenius_norm <- function(mat) {
  frobenius_norm <- sqrt(sum(mat^2))
  return(frobenius_norm)
}

#' Calculate the spectral norm of a matrix.
#'
#' This function calculates the spectral norm of a matrix.
#' @param mat A matrix.
#' @return The spectral norm of the matrix.
#' @examples
#' mat <- matrix(c(1, 2, 3, 4), nrow = 2)
#' matrix_spectral_norm(mat)
#' @export
matrix_spectral_norm <- function(mat) {
  eigen_result <- eigen(mat)
  spectral_norm <- max(abs(eigen_result$values))
  return(spectral_norm)
}

#' Calculate the rank of a matrix.
#'
#' This function calculates the rank of a matrix.
#' @param mat A matrix.
#' @return The rank of the matrix.
#' @examples
#' mat <- matrix(c(1, 2, 3, 4), nrow = 2)
#' matrix_rank(mat)
#' @export
matrix_rank <- function(mat) {
  rank_mat <- rankMatrix(mat)
  return(rank_mat)
}
