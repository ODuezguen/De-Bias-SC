#' This function calculates the bias term given the following inputs:
#' 
#' @params X0: This is a (k x n0) matrix of feature values for the control group.
#' @params X1: This is a (k x n1) matrix of feature values for the treatment group.
#' @params Y: This is a (T x n1+n0) matrix of values that we want to predict using the feature matrices X0 and X1.
#' @params D: This is a binary vector of length (n1+n0) indicating membership in one of the two groups.
#' @params W: This is a (n1 x n0) matrix of weights that will be used to adjust the predicted values.
#'
#' @return It returns the bias of the aggregated synthetic control (see Abadie and L'Hour (2021) for more details)
#'
#' @author Onur Düzgün

de.bias <- function(X0, X1, Y, D, W){
  
  # Adding some checks
  if (!is.matrix(X0) || !is.matrix(X1) || !is.matrix(Y) || !is.vector(D)) {
    stop("Inputs must be matrices and D must be a vector")
  }
  if (nrow(X0) != nrow(X1)) {
    stop("X0 and X1 must have the same number of rows")
  }
  if (length(D) != ncol(X0) + ncol(X1)) {
    stop("D must have the same length as the total number of columns in X0 and X1")
  }
  
  # Creating augmented feature matrices by setting the last element of each column to 1
  augmented_X0 <- cbind(X0, rep(1, ncol(X0)))
  augmented_X1 <- cbind(X1, rep(1, ncol(X1)))
  
  # Calculating the weight matrix using the inverse of the transpose of augmented_X0 multiplied by augmented_X_0
  weight_matrix <- tryCatch(
    solve(t(augmented_X0) %*% augmented_X0) %*% augmented_X0,
    error = function(e) {
      warning("Error calculating weight matrix. Using default value.")
      return(matrix(1, nrow(X0), ncol(X0)))
    }
  )
  
  # Pre-allocating dbias and calculating it
  dbias = matrix(nrow=nrow(Y),ncol=ncol(X1))
  mean0 <- weight_matrix %*% Y[,D==0]
  dbias <- t(augmented_X1) %*% mean0 - W %*% (t(augmented_X0) %*% mean0)
  
  return(dbias)
}
