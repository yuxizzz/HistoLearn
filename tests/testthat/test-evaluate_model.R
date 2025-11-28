# Simulate simple two-class training and test data
set.seed(42)

X_train <- rbind(
  matrix(rnorm(100, mean = 0, sd = 1), nrow = 20, ncol = 5),
  matrix(rnorm(100, mean = 3, sd = 1), nrow = 20, ncol = 5)
)
y_train <- factor(c(rep("A", 20), rep("B", 20)))
trainset <- load_embeddings(X_train, y_train)

X_test <- rbind(
  matrix(rnorm(50, mean = 0, sd = 1), nrow = 10, ncol = 5),
  matrix(rnorm(50, mean = 3, sd = 1), nrow = 10, ncol = 5)
)
y_test <- factor(c(rep("A", 10), rep("B", 10)))
testset <- load_embeddings(X_test, y_test)

# Fit k-NN model for evaluation tests
model <- train_model(trainset, dr = "pca", dr_k = 2, model = "knn")

# Tests: evaluate_model output structure and types
test_that("evaluate_model returns confusion matrices and numeric metrics", {
  output <- evaluate_model(trained_model = model, test_data = testset)

  expect_type(output, "list")
  expect_true(
    all(
      c(
        "train_conf_matrix", "train_metric",
        "test_conf_matrix", "test_metric"
      ) %in% names(output)
    )
  )

  expect_s3_class(output$train_conf_matrix, "ggplot")
  expect_s3_class(output$test_conf_matrix, "ggplot")

  expect_true(is.numeric(output$train_metric))
  expect_true(length(output$train_metric) == 1)
  expect_true(output$train_metric >= 0 && output$train_metric <= 1)

  expect_true(is.numeric(output$test_metric))
  expect_true(length(output$test_metric) == 1)
  expect_true(output$test_metric >= 0 && output$test_metric <= 1)
})

# Tests: evaluate_model error handling for invalid inputs
test_that("evaluate_model errors on invalid input types", {
  # wrong test_data type
  expect_error(
    evaluate_model(trained_model = model, test_data = list()),
    "`test_data` must be a 'histofeature' object"
  )

  # wrong trained_model type
  expect_error(
    evaluate_model(trained_model = list(), test_data = testset),
    "`trained_model` must be a 'histolearn' object"
  )
})

# [END]
