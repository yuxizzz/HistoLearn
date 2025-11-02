test_that("load_embeddings works with valid feature and label (base case)", {
  feature <- matrix(1:6, nrow = 3, ncol = 2)
  label <- c("A", "B", "A")

  result <- load_embeddings(feature, label)

  expect_s3_class(result, "histofeature")
  expect_type(result, "list")
  expect_named(result, c("feature", "label"))
  expect_true(is.data.frame(result$feature))
  expect_true(is.factor(result$label))

  expect_equal(nrow(result$feature), 3)
  expect_equal(ncol(result$feature), 2)
  expect_equal(levels(result$label), c("A", "B"))
})

test_that("load_embeddings handles NULL label", {
  feature <- matrix(1:4, nrow = 2, ncol = 2)
  result <- load_embeddings(feature, label = NULL)

  expect_null(result$label)
  expect_true(is.data.frame(result$feature))
})

test_that("load_embeddings accepts label as data.frame or matrix", {
  feature <- matrix(1:6, nrow = 3, ncol = 2)
  label_df <- data.frame(label = c("X", "Y", "X"))
  label_mat <- matrix(c("X", "Y", "X"), ncol = 1)
  res_df <- load_embeddings(feature, label_df)
  res_mat <- load_embeddings(feature, label_mat)

  expect_equal(res_df$label, res_mat$label)
})

test_that("load_embeddings accepts feature as data.frame or matrix", {
  feature <- matrix(1:6, nrow = 3, ncol = 2)
  feature_df <- as.data.frame(feature)
  label_df <- data.frame(label = c("X", "Y", "X"))
  res_df <- load_embeddings(feature_df, label_df)
  res_mat <- load_embeddings(feature, label_df)

  expect_equal(res_df$feature, res_mat$feature)
})

test_that("load_embeddings fails if feature and label lengths mismatch", {
  feature <- matrix(1:4, nrow = 2)
  label <- c("A", "B", "C")

  expect_error(load_embeddings(feature, label))
})
