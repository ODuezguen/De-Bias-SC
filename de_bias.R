#' This function calculates the bias term given the inputs:
#' 
#' X_0: This is a matrix of feature values for the control group.
#' X_1: This is a matrix of feature values for the treatment group.
#' Y: This is a vector of values that we want to predict using the feature matrices X_0 and X_1.
#' D: This is a binary vector indicating membership in one of the two groups represented by X_0 and X_1.
#' weights: This is a n1 x n0 matrix of weights that will be used to adjust the predicted values.

de_bias <- function(X_0, X_1, Y, D, weights){
  # Calculate the number of columns in each matrix using colSums
  num_cols_0 <- colSums(X_0 != 0)
  num_cols_1 <- colSums(X_1 != 0)
  
  # Create augmented feature matrices by setting the last element of each column to 1
  augmented_X_0 <- cbind(X_0, 1)
  augmented_X_1 <- cbind(X_1, 1)
  
  # Calculate the weight matrix using the inverse of the transpose of augmented_X_0 multiplied by augmented_X_0
  weight_matrix <- solve(t(augmented_X_0) %*% augmented_X_0) %*% augmented_X_0
  
  # Vectorize the calculation of bias
  mean_0 <- weight_matrix %*% Y[D==0]
  de_bias <- t(augmented_X_1) %*% mean_0 - weights %*% (t(augmented_X_0) %*% mean_0)
  
  return(de_bias)
}
