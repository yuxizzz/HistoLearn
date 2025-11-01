test_that("load_embeddings returns correct structure", {
  data(train_embeddings)
  data(train_labels)

  res <- load_embeddings(train_embeddings, train_labels)

  expect_type(res, "list")
  expect_s3_class(res$feature, "data.frame")
  expect_true(is.factor(res$label))
  expect_equal(nrow(res$feature), length(res$label))
})
