#' This function takes X0 and X1 and returns two normalized matrices by dividing the elements in X0 and X1 by the square root of their row variances. 
#' Remember: to normalize a vector, you divide each element by the square root of the sum of the squares.
#' The function also includes checks and error handling.
#'
#' X0: A (k x n0) matrix of observables of the control group.
#' X1: A (k x n1) matrix of observables of the treatment group.


normalize <- function(X0, X1){
  # Adding some checks
  if (!is.matrix(X0) && !is.data.frame(X0)) {
    stop("X0 must be a matrix or data frame")
  }
  if (!is.matrix(X1) && !is.data.frame(X1)) {
    stop("X1 must be a matrix or data frame")
  }
  if (nrow(X0) != nrow(X1)) {
    stop("X0 and X1 must have the same number of rows")
  }

  # Normalizing X
  nVar <- dim(X0)[1]
  combined_X <- cbind(X0, X1)
  variance <- sqrt(diag(cov(combined_X)))
  
  # Handle cases where the variance is zero
  if (any(variance == 0)) {
    stop("The variance of one or more rows is zero. Cannot normalize data. Note: all variables must be time-varying.")
  }

  X0_norm <- t(t(X0) %*% ( 1/variance * diag(rep(nVar, 1)) ))
  X1_norm <- t(t(X1) %*% ( 1/variance * diag(rep(nVar, 1)) ))
  
  return(X0_norm, X1_norm)
}
