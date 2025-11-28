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


test_that("load_embeddings requires labels", {
  feature <- matrix(1:4, nrow = 2, ncol = 2)
  expect_error(load_embeddings(feature, label = NULL))
})

test_that("load_embeddings accepts label as data.frame, factor, matrix or vector", {
  feature   <- matrix(1:6, nrow = 3, ncol = 2)
  label_vec <- c("X", "Y", "X")

  label_df  <- data.frame(label = label_vec)
  label_mat <- matrix(label_vec, ncol = 1)
  label_fac <- factor(label_vec)

  res_vec <- load_embeddings(feature, label_vec)
  res_df  <- load_embeddings(feature, label_df)
  res_mat <- load_embeddings(feature, label_mat)
  res_fac <- load_embeddings(feature, label_fac)

  # All should produce identical factor labels
  expect_identical(res_vec$label, res_df$label)
  expect_identical(res_vec$label, res_mat$label)
  expect_identical(res_vec$label, res_fac$label)

  # And they should be factors with expected levels
  expect_true(is.factor(res_vec$label))
  expect_equal(levels(res_vec$label), c("X", "Y"))
})

test_that("load_embeddings accepts feature as data.frame or matrix", {
  feature <- matrix(1:6, nrow = 3, ncol = 2)
  feature_df <- as.data.frame(feature)
  label_df <- data.frame(label = c("X", "Y", "X"))
  res_df <- load_embeddings(feature_df, label_df)
  res_mat <- load_embeddings(feature, label_df)

  expect_equal(res_df$feature, res_mat$feature)
})

test_that("load_embeddings errors when label has more than one column", {
  feature   <- matrix(1:6, nrow = 3, ncol = 2)
  label_mat <- matrix(c("A", "B", "A", "x", "y", "z"), ncol = 2)
  expect_error(load_embeddings(feature, label_mat))
})

test_that("load_embeddings fails if feature and label lengths mismatch", {
  feature <- matrix(1:4, nrow = 2)
  label <- c("A", "B", "C")
  expect_error(load_embeddings(feature, label))
})

test_that("load_embeddings fails if feature matrix is in valid", {
  feature <- matrix(1:4, nrow = 2, ncol = 2)
  labels <- c("A", "B")
  feature[1, 2] <- NA
  expect_error(load_embeddings(feature, label = labels))
  feature[1, 2] <- Inf
  expect_error(load_embeddings(feature, label = labels))
})
# [END]
