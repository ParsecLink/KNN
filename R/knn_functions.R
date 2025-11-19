#' K-Nearest Neighbors Classification (pure R)
#'
#' This function implements the K-Nearest Neighbors algorithm for classification
#' using pure R code.
#' @importFrom stats model.frame terms delete.response model.response model.matrix
#' @param train A numeric matrix or data frame of training predictors (rows = observations).
#' @param test A numeric matrix or data frame of test predictors.
#' @param cl Factor or character vector of true classifications of training set.
#' @param k Number of neighbors to consider (positive integer).
#' @param distance_method Method for distance calculation: "euclidean" (default) or "manhattan".
#' @param tie_method How to break ties: "alphabetical" (default), "first", or "random".
#' @return Factor of classifications for test set (levels preserved from \code{cl}).
#' @export
#' @examples
#' data(iris)
#' set.seed(123)
#' train_idx <- sample(seq_len(nrow(iris)), 100)
#' train <- iris[train_idx, 1:4]
#' test <- iris[-train_idx, 1:4]
#' train_labels <- iris[train_idx, 5]
#' predictions <- knn_r(train, test, train_labels, k = 5)
#' table(predictions, iris[-train_idx, 5])
knn_r <- function(train, test, cl, k = 1, distance_method = "euclidean",
                  tie_method = c("alphabetical", "first", "random")) {
  tie_method <- match.arg(tie_method)

  # Basic type checks
  if (!is.matrix(train) && !is.data.frame(train)) {
    stop("`train` must be a matrix or data.frame.")
  }
  if (!is.matrix(test) && !is.data.frame(test)) {
    stop("`test` must be a matrix or data.frame.")
  }

  train <- as.matrix(train)
  test  <- as.matrix(test)

  if (ncol(train) != ncol(test)) {
    stop("`train` and `test` must have the same number of columns/features.")
  }

  if (length(cl) != nrow(train)) {
    stop("`cl` must have length equal to nrow(train).")
  }

  if (!is.factor(cl)) cl <- as.factor(cl)

  if (length(k) != 1L || !is.numeric(k) || is.na(k)) stop("`k` must be a single positive integer.")
  k <- as.integer(k)
  if (k < 1L) stop("`k` must be >= 1.")
  if (k > nrow(train)) stop("`k` cannot be larger than the number of training samples.")

  if (!distance_method %in% c("euclidean", "manhattan")) {
    stop("`distance_method` must be 'euclidean' or 'manhattan'.")
  }

  if (anyNA(train) || anyNA(test)) stop("NA values found in `train` or `test`; handle them before calling knn_r.")
  if (anyNA(cl)) stop("NA values found in `cl`; handle them before calling knn_r.")

  n_test <- nrow(test)
  predictions <- character(n_test)

  for (i in seq_len(n_test)) {
    # compute distances
    if (distance_method == "euclidean") {
      diffs <- train - matrix(test[i, , drop = FALSE], nrow = nrow(train), ncol = ncol(train), byrow = TRUE)
      distances <- sqrt(rowSums(diffs * diffs))
    } else {
      distances <- rowSums(abs(train - matrix(test[i, , drop = FALSE], nrow = nrow(train), ncol = ncol(train), byrow = TRUE)))
    }

    # indices of k nearest neighbors
    nearest_idx <- order(distances, decreasing = FALSE)[seq_len(k)]
    nearest_labels <- cl[nearest_idx]

    # majority vote
    tbl <- table(nearest_labels)
    max_count <- max(tbl)
    winners <- names(tbl)[tbl == max_count]

    chosen <- if (length(winners) == 1L) {
      winners[1]
    } else {
      switch(tie_method,
             alphabetical = sort(winners)[1],
             first = winners[1],
             random = sample(winners, 1))
    }

    predictions[i] <- chosen
  }

  factor(predictions, levels = levels(cl))
}


#' KNN Predict Function (formula interface)
#'
#' A more user-friendly interface for KNN predictions using a formula.
#' @importFrom stats model.frame model.response terms model.matrix delete.response
#' @param formula A formula of the form response ~ predictors.
#' @param data Training data frame.
#' @param newdata Test data frame to predict.
#' @param k Number of neighbors (positive integer).
#' @param distance_method Distance method: "euclidean" or "manhattan".
#' @return Factor of predictions (length = nrow(newdata)).
#' @importFrom stats model.frame terms delete.response model.response
#' @export
#' @examples
#' data(iris)
#' set.seed(123)
#' train_idx <- sample(seq_len(nrow(iris)), 100)
#' train_data <- iris[train_idx, ]
#' test_data <- iris[-train_idx, ]
#' pred <- knn_predict(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
#'                     train_data, test_data, k = 3)
#' table(pred, test_data$Species)
knn_predict <- function(formula, data, newdata, k = 1, distance_method = "euclidean") {
  if (missing(formula) || missing(data) || missing(newdata)) {
    stop("`formula`, `data`, and `newdata` must be supplied.")
  }

  # Build model frame for training to extract response and to capture terms/contrasts
  mf_train <- model.frame(formula, data = data)
  response <- model.response(mf_train)
  if (is.null(response)) stop("No response found in 'data' for the provided formula.")
  train_y <- if (is.factor(response)) response else as.factor(response)

  # Terms object (captures formula structure & contrasts)
  terms_obj <- terms(formula, data = data)

  # model.matrix ensures consistent design (dummy variables / contrasts)
  X_train <- model.matrix(delete.response(terms_obj), data = data)
  # For newdata, build model matrix using the same terms
  X_new <- model.matrix(delete.response(terms_obj), data = newdata)

  cn_train <- colnames(X_train)
  cn_new <- colnames(X_new)

  if (!identical(cn_train, cn_new)) {
    missing_in_new <- setdiff(cn_train, cn_new)
    extra_in_new <- setdiff(cn_new, cn_train)

    if (length(missing_in_new) > 0) {
      # add missing columns to X_new as zeros in the correct order
      add_mat <- matrix(0, nrow = nrow(X_new), ncol = length(missing_in_new),
                        dimnames = list(NULL, missing_in_new))
      X_new <- cbind(X_new, add_mat)
    }
    if (length(extra_in_new) > 0) {
      # drop unexpected columns in X_new
      X_new <- X_new[, setdiff(colnames(X_new), extra_in_new), drop = FALSE]
    }
    # reorder to match training columns
    X_new <- X_new[, cn_train, drop = FALSE]
  }

  knn_r(X_train, X_new, cl = train_y, k = k, distance_method = distance_method)
}
