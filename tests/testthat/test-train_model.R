set.seed(4)
X <- matrix(rnorm(300), nrow = 60, ncol = 5)
y <- factor(sample(c("A", "B"), size = 60, replace = TRUE))
hf <- load_embeddings(X, y)
test_that("train_model runs dimension reduction and supervised ml
          and returns histolearn object", {
  res <- train_model(
    feature_embedding = hf,
    dr = "pca",
    dr_k = 3,
    model = "knn"
  )

  expect_type(res, "list")
  expect_s3_class(res, "histolearn")
  expect_true(all(c("dr_model", "model", "train_pred", "method", "dr_dim")
                  %in% names(res)))

  expect_s3_class(res$dr_model, "prcomp")
  expect_equal(res$dr_dim, 3)
  expect_s3_class(res$model, "knn3")
  expect_true(is.factor(res$train_pred))
  expect_length(res$train_pred, nrow(X))
  expect_identical(res$method, c("pca", "knn"))
})

test_that("train_model handles invalid input and unsupported model", {
  bad <- list(feature = matrix(rnorm(20), nrow = 10), label = rep("A", 10))
  expect_error(train_model(bad))

  hf_nolabel <- load_embeddings(feature=matrix(rnorm(20), nrow = 10), label=NULL)
  expect_error(train_model(hf_nolabel))

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
  model <- train_knn(X, y)
  expect_s3_class(model, "knn3")
  preds <- predict(model, X, type = "class")
  expect_true(is.factor(preds))
  expect_length(preds, nrow(X))
})

