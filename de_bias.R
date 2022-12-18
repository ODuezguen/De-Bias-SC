#' This function calculates the bias term given the inputs:
#' 
#' X0: This is a matrix of feature values for the control group.
#' X1: This is a matrix of feature values for the treatment group.
#' Y: This is a vector of values that we want to predict using the feature matrices X0 and X1.
#' D: This is a binary vector indicating membership in one of the two groups represented by X0 and X1.
#' weights: This is a n1 x n0 matrix of weights that will be used to adjust the predicted values.

de_bias <- function(X0, X1, Y, D, weights){
  # Calculate the number of columns in each matrix using colSums
  num_cols0 <- colSums(X0 != 0)
  num_cols1 <- colSums(X1 != 0)
  
  # Create augmented feature matrices by setting the last element of each column to 1
  augmented_X0 <- cbind(X0, 1)
  augmented_X1 <- cbind(X1, 1)
  
  # Calculate the weight matrix using the inverse of the transpose of augmented_X0 multiplied by augmented_X_0
  weight_matrix <- solve(t(augmented_X0) %*% augmented_X0) %*% augmented_X0
  
  # Vectorize the calculation of bias
  mean0 <- weight_matrix %*% Y[D==0]
  de_bias <- t(augmented_X1) %*% mean0 - weights %*% (t(augmented_X0) %*% mean0)
  
  return(de_bias)
}
