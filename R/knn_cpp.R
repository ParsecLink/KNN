#' K-Nearest Neighbors Classification using Rcpp
#'
#' This function implements the K-Nearest Neighbors algorithm for classification
#' using Rcpp for improved performance.
#'
#' @param train A matrix or data frame of training data
#' @param test A matrix or data frame of test data  
#' @param cl Factor of true classifications of training set
#' @param k Number of neighbors to consider
#' @param distance_method Method for distance calculation: "euclidean" (default) or "manhattan"
#' @return Factor of classifications for test set
#' @export
#' @examples
#' data(iris)
#' set.seed(123)
#' train_idx <- sample(1:nrow(iris), 100)
#' train <- iris[train_idx, 1:4]
#' test <- iris[-train_idx, 1:4]
#' train_labels <- iris[train_idx, 5]
#' predictions <- knn_cpp(train, test, train_labels, k = 5)
knn_cpp <- function(train, test, cl, k = 1, distance_method = "euclidean") {
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
  
  # Use Rcpp implementation if available, otherwise fall back to R
  if (require(Rcpp) && exists("knn_cpp_impl")) {
    predictions <- knn_cpp_impl(train_matrix, test_matrix, as.character(cl), k, distance_method)
  } else {
    warning("Rcpp not available, using R implementation")
    return(knn_r(train, test, cl, k, distance_method))
  }
  
  return(factor(predictions, levels = levels(cl)))
}

