#' K-Nearest Neighbors Classification using Rcpp
#' This function implements the K-Nearest Neighbors algorithm for classification
#' using Rcpp for improved performance.

knn_cpp_wrapper <- function(train, test, cl, k = 1, distance_method = "euclidean") {
  # Input validation
  if (!is.matrix(train) && !is.data.frame(train)) {
    stop("train must be a matrix or data frame")
  }
  if (!is.matrix(test) && !is.data.frame(test)) {
    stop("test must be a matrix or data frame")
  }
  if (ncol(train) != ncol(test)) {
    stop("train and test must have the same number of columns")
  }
  if (length(cl) != nrow(train)) {
    stop("cl must have the same length as the number of rows in train")
  }
  if (k > nrow(train)) {
    stop("k cannot be larger than the number of training samples")
  }
  if (k < 1) {
    stop("k must be a positive integer")
  }

  train_matrix <- as.matrix(train)
  test_matrix <- as.matrix(test)

  predictions <- knn_cpp(train_matrix, test_matrix, as.character(cl), k, distance_method)

  return(factor(predictions, levels = levels(cl)))
}
