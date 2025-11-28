# Purpose: test for visualization function
# Author: Yuxi Zhu
# Date: 2025-11-27
# Version: 0.1.0
# Bugs and Issues: None known.

# Simulate simple two-class data
set.seed(2)
X <- matrix(rnorm(200), nrow = 40, ncol = 10)
y <- factor(sample(c("A", "B"), size = 40, replace = TRUE))
hf <- load_embeddings(X, y)

test_that("visualize_embeddings returns ggmatrix when (2 < k <= 8) and
          returns ggplot when (k = 2)", {
  p <- visualize_embeddings(hf, dimensions = 3, type = "pca")
  expect_s3_class(p, "ggmatrix")
  p <- visualize_embeddings(hf, dimensions = 2, type = "pca")
  expect_s3_class(p, "ggplot")
})

test_that("visualize_embeddings errors on invalid input class", {
  bad_input <- list(feature = matrix(rnorm(20), nrow = 10), label = rep("A", 10))
  expect_error(visualize_embeddings(bad_input, dimensions = 2, type = "pca"))
  bad_label <- list(feature = matrix(rnorm(20), nrow = 10), label = NULL)
  expect_error(visualize_embeddings(bad_label, dimensions = 2, type = "pca"))
})

test_that("visualize_embeddings errors on too many or too little dimensions", {
  expect_error(visualize_embeddings(hf, dimensions = 11, type = "pca"))
  expect_error(visualize_embeddings(hf, dimensions = 1, type = "pca"))})

test_that("visualize_embeddings errors on unsupported type", {
  expect_error(visualize_embeddings(hf, dimensions = 2, type = "umap"))
})

# [END]
