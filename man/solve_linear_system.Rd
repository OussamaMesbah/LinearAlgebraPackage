% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/additional.R
\name{solve_linear_system}
\alias{solve_linear_system}
\title{Solve a linear system of equations.}
\usage{
solve_linear_system(A, b)
}
\arguments{
\item{A}{Coefficient matrix.}

\item{b}{Vector of constants.}
}
\value{
The solution vector.
}
\description{
This function takes a coefficient matrix and a vector of constants as input and solves the linear system.
}
\examples{
A <- matrix(c(1, 2, 3, 4), nrow = 2)
b <- c(5, 6)
solve_linear_system(A, b)
}
