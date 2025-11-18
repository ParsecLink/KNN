test_that("knn_predict works with formula interface", {
  data(iris)
  set.seed(123)
  train_idx <- sample(1:nrow(iris), 100)

  predictions <- knn_predict(Species ~ ., iris[train_idx, ], iris[-train_idx, ], k = 3)

  expect_length(predictions, 50)
  expect_s3_class(predictions, "factor")
  expect_equal(levels(predictions), levels(iris$Species))
})

test_that("knn_predict works with subset of predictors", {
  data(iris)
  set.seed(123)
  train_idx <- sample(1:nrow(iris), 100)

  # Use only two predictors
  predictions <- knn_predict(Species ~ Sepal.Length + Petal.Length,
                             iris[train_idx, ], iris[-train_idx, ], k = 3)

  expect_length(predictions, 50)
  expect_s3_class(predictions, "factor")
})

test_that("knn_predict handles formula errors", {
  data(iris)
  set.seed(123)
  train_idx <- sample(1:nrow(iris), 100)

  # Invalid formula
  expect_error(knn_predict(InvalidVar ~ ., iris[train_idx, ], iris[-train_idx, ], k = 3))
})

test_that("knn_predict and knn_r give same results", {
  data(iris)
  set.seed(123)
  train_idx <- sample(1:nrow(iris), 100)
  train_data <- iris[train_idx, ]
  test_data <- iris[-train_idx, ]

  # Using formula interface
  predictions_formula <- knn_predict(Species ~ ., train_data, test_data, k = 3)

  # Using direct interface
  predictions_direct <- knn_r(train_data[, 1:4], test_data[, 1:4], train_data$Species, k = 3)

  # Should give same results
  expect_equal(predictions_formula, predictions_direct)
})
