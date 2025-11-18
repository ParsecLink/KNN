#' K-Nearest Neighbors Classification
#'
#' This function implements the K-Nearest Neighbors algorithm for classification
#' using pure R code.
#'
#' @param train A matrix or data frame of training data
#' @param test A matrix or data frame of test data
#' @param cl Factor of true classifications of training set
#' @param k Number of neighbors to consider
#' @param distance_method Method for distance calculation: "euclidean" (default) or "manhattan"
#' @return Factor of classifications for test set
#' @export
#' @examples
#' # Basic usage with iris dataset
#' data(iris)
#' set.seed(123)
#' train_idx <- sample(1:nrow(iris), 100)
#' train <- iris[train_idx, 1:4]
#' test <- iris[-train_idx, 1:4]
#' train_labels <- iris[train_idx, 5]
#' predictions <- knn_r(train, test, train_labels, k = 5)
#' table(predictions, iris[-train_idx, 5])
#'
#' # Using different distance methods
#' pred_euclidean <- knn_r(train, test, train_labels, k = 3, distance_method = "euclidean")
#' pred_manhattan <- knn_r(train, test, train_labels, k = 3, distance_method = "manhattan")
knn_r <- function(train, test, cl, k = 1, distance_method = "euclidean") {
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

  train <- as.matrix(train)
  test <- as.matrix(test)
  cl <- as.factor(cl)

  predictions <- character(nrow(test))

  for (i in 1:nrow(test)) {
    # Calculate distances
    if (distance_method == "euclidean") {
      distances <- sqrt(rowSums((train - matrix(test[i, ], nrow = nrow(train), ncol = ncol(train), byrow = TRUE))^2))
    } else if (distance_method == "manhattan") {
      distances <- rowSums(abs(train - matrix(test[i, ], nrow = nrow(train), ncol = ncol(train), byrow = TRUE)))
    } else {
      stop("Invalid distance method. Use 'euclidean' or 'manhattan'")
    }

    # Get k nearest neighbors
    nearest_indices <- order(distances)[1:k]
    nearest_labels <- cl[nearest_indices]

    # Majority vote
    label_counts <- table(nearest_labels)
    max_count <- max(label_counts)
    winning_labels <- names(label_counts)[label_counts == max_count]

    # Handle ties randomly
    if (length(winning_labels) > 1) {
      winning_label <- sample(winning_labels, 1)
    } else {
      winning_label <- winning_labels[1]
    }

    predictions[i] <- winning_label
  }

  return(factor(predictions, levels = levels(cl)))
}

#' KNN Predict Function
#'
#' A more user-friendly interface for KNN predictions
#'
#' @param formula A formula of the form class ~ predictors
#' @param data Training data
#' @param newdata Test data
#' @param k Number of neighbors
#' @param distance_method Distance method
#' @return Factor of predictions
#' @export
#' @examples
#' # Formula interface with iris dataset
#' data(iris)
#' set.seed(123)
#' train_idx <- sample(1:nrow(iris), 100)
#' train_data <- iris[train_idx, ]
#' test_data <- iris[-train_idx, ]
#' pred <- knn_predict(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
#'                     train_data, test_data, k = 3)
#' table(pred, test_data$Species)
#'
#' # Using only specific predictors
#' pred_subset <- knn_predict(Species ~ Sepal.Length + Petal.Length,
#'                           train_data, test_data, k = 3)
knn_predict <- function(formula, data, newdata, k = 1, distance_method = "euclidean") {
  # Extract response and predictors
  model_frame <- model.frame(formula, data)
  train_response <- model_frame[, 1]
  train_predictors <- model_frame[, -1, drop = FALSE]

  # For newdata, we need to ensure it has the same structure
  terms_obj <- terms(formula, data = data)
  newdata_frame <- model.frame(delete.response(terms_obj), newdata)

  knn_r(train_predictors, newdata_frame, train_response, k, distance_method)
}
