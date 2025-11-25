set.seed(4)
X <- matrix(rnorm(300), nrow = 60, ncol = 5)
y <- factor(sample(c("A", "B"), size = 60, replace = TRUE))
hf <- load_embeddings(X, y)
test_that("train_model runs dimension reduction and knn
          and returns histolearn object", {
  res <- train_model(
    feature_embedding = hf,
    dr = "pca",
    dr_k = 3,
    model = "knn"
  )
  expect_type(res, "list")
  expect_s3_class(res, "histolearn")

  expect_true(all(
    c("dr_model", "model", "train_cm", "train_acc", "method", "dr_dim") %in% names(res)
  ))

  expect_s3_class(res$dr_model, "prcomp")
  expect_equal(res$dr_dim, 3L)
  expect_s3_class(res$model, "train")
  expect_equal(res$model$method, "knn")
  expect_s3_class(res$train_cm, "confusionMatrix")
  expect_true(is.numeric(res$train_acc))
  expect_true(res$train_acc >= 0 && res$train_acc <= 1)
  expect_identical(res$method, c("pca", "knn"))
})

test_that("train_model runs dimension reduction and logistic classifier", {
  res <- train_model(
    feature_embedding = hf,
    dr   = "pca",
    dr_k = 3,
    model = "logistic"
  )

  expect_type(res, "list")
  expect_s3_class(res, "histolearn")
  expect_true(all(
    c("dr_model", "model", "train_cm", "train_acc", "method", "dr_dim") %in% names(res)
  ))

  expect_s3_class(res$dr_model, "prcomp")
  expect_equal(res$dr_dim, 3L)

  expect_s3_class(res$model, "train")
  expect_equal(res$model$method, "multinom")
  expect_s3_class(res$train_cm, "confusionMatrix")
  expect_true(is.numeric(res$train_acc))
  expect_true(res$train_acc >= 0 && res$train_acc <= 1)
  expect_identical(res$method, c("pca", "logistic"))
})

test_that("train_model handles invalid input and unsupported model", {
  bad <- list(feature = matrix(rnorm(20), nrow = 10), label = rep("A", 10))
  expect_error(train_model(bad))
  expect_error(train_model(hf, model = "svm"))
  expect_error(train_model(hf, dr = "umap"))
})

## Test for internal reduce dim
set.seed(1)
X <- matrix(rnorm(200), nrow = 40, ncol = 5)
test_that("reduce_dim returns prcomp model and k columns", {
  res <- reduce_dim(X, method = "pca", k = 3)

  expect_type(res, "list")
  expect_true(all(c("model", "X_red") %in% names(res)))
  expect_s3_class(res$model, "prcomp")
  expect_true(is.matrix(res$X_red))
  expect_equal(nrow(res$X_red), nrow(X))
  expect_equal(ncol(res$X_red), 3)
})

test_that("reduce_dim errors on k greater than input dimension", {
  expect_error(reduce_dim(X, method = "pca", k = 10))
})

test_that("reduce_dim errors on unsupported method", {
  expect_error(reduce_dim(X, method = "umap", k = 2))
})

## Test for internal use train_knn
test_that("train_knn trains a valid KNN model", {
  X <- matrix(rnorm(200), nrow = 40, ncol = 5)
  y <- factor(sample(c("A", "B"), size = 40, replace = TRUE))
  X_df <- as.data.frame(X)
  colnames(X_df) <- paste0("x", seq_len(ncol(X_df)))
  model <- train_knn(X_df, y)
  expect_s3_class(model, "train")
  expect_equal(model$method, "knn")
  preds <- predict(model, X_df)
  expect_true(is.factor(preds))
  expect_length(preds, nrow(X_df))
})

test_that("train_logistic trains a valid logistic regression model.", {
  X_log <- matrix(rnorm(200), nrow = 40, ncol = 5)
  y_log <- factor(sample(c("A", "B"), size = 40, replace = TRUE))

  X_df <- as.data.frame(X_log)
  colnames(X_df) <- paste0("x", seq_len(ncol(X_df)))
  model <- train_logistic(X_df, y_log)
  # caret::train object with method = "glm"
  expect_s3_class(model, "train")
  expect_equal(model$method, "multinom")
  # Predictions on training data
  preds <- predict(model, X_df)
  expect_true(is.factor(preds))
  expect_length(preds, nrow(X_df))
})
