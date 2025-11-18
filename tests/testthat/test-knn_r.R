test_that("knn_r works with iris dataset", {
  data(iris)
  set.seed(123)
  train_idx <- sample(1:nrow(iris), 100)
  train <- iris[train_idx, 1:4]
  test <- iris[-train_idx, 1:4]
  train_labels <- iris[train_idx, 5]

  predictions <- knn_r(train, test, train_labels, k = 3)

  expect_type(predictions, "integer")
  expect_length(predictions, 50)
  expect_s3_class(predictions, "factor")
  expect_equal(levels(predictions), levels(iris$Species))
})

test_that("knn_r handles invalid inputs", {
  data(iris)

  # Different number of columns
  expect_error(knn_r(iris[1:10, 1:4], iris[11:20, 1:3], iris[1:10, 5]))

  # Different number of rows in labels
  expect_error(knn_r(iris[1:10, 1:4], iris[11:20, 1:4], iris[1:9, 5]))

  # k too large
  expect_error(knn_r(iris[1:10, 1:4], iris[11:20, 1:4], iris[1:10, 5], k = 11))

  # k too small
  expect_error(knn_r(iris[1:10, 1:4], iris[11:20, 1:4], iris[1:10, 5], k = 0))

  # Invalid distance method
  expect_error(knn_r(iris[1:10, 1:4], iris[11:20, 1:4], iris[1:10, 5], distance_method = "invalid"))
})

test_that("knn_r works with different distance methods", {
  data(iris)
  set.seed(123)
  train_idx <- sample(1:nrow(iris), 30)
  train <- iris[train_idx, 1:4]
  test <- iris[131:140, 1:4]  # Small test set
  train_labels <- iris[train_idx, 5]

  predictions_euclidean <- knn_r(train, test, train_labels, k = 3, distance_method = "euclidean")
  predictions_manhattan <- knn_r(train, test, train_labels, k = 3, distance_method = "manhattan")

  expect_length(predictions_euclidean, 10)
  expect_length(predictions_manhattan, 10)
  expect_s3_class(predictions_euclidean, "factor")
  expect_s3_class(predictions_manhattan, "factor")
})

test_that("knn_r handles different k values", {
  data(iris)
  set.seed(123)
  train_idx <- sample(1:nrow(iris), 30)
  train <- iris[train_idx, 1:4]
  test <- iris[131:135, 1:4]  # Very small test set
  train_labels <- iris[train_idx, 5]

  predictions_k1 <- knn_r(train, test, train_labels, k = 1)
  predictions_k3 <- knn_r(train, test, train_labels, k = 3)
  predictions_k5 <- knn_r(train, test, train_labels, k = 5)

  expect_length(predictions_k1, 5)
  expect_length(predictions_k3, 5)
  expect_length(predictions_k5, 5)
})
