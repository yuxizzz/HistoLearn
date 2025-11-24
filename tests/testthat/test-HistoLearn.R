test_that("Integration test: test end-to-end pipeline runs on example data", {
  # Load package and example data
  library(HistoLearn)

  data("train_embeddings", package = "HistoLearn")
  data("train_labels",     package = "HistoLearn")
  data("test_embeddings",  package = "HistoLearn")
  data("test_labels",      package = "HistoLearn")

  # 1) Construct histofeature objects
  trainset <- load_embeddings(
    feature = train_embeddings,
    label   = train_labels
  )

  testset <- load_embeddings(
    feature = test_embeddings,
    label   = test_labels
  )

  expect_s3_class(trainset, "histofeature")
  expect_s3_class(testset,  "histofeature")

  expect_true(is.data.frame(trainset$feature))
  expect_true(is.factor(trainset$label))
  expect_equal(nrow(trainset$feature), length(trainset$label))
  expect_equal(ncol(trainset$feature), ncol(testset$feature))  # same feature dimension

  # 2) Visualization works (PCA)
  p2 <- visualize_embeddings(
    input_data = trainset,
    dimensions = 2,
    type       = "pca"
  )
  expect_true(inherits(p2, "ggplot") || inherits(p2, "ggmatrix"))

  p4 <- visualize_embeddings(
    input_data = trainset,
    dimensions = 4,
    type       = "pca"
  )
  expect_true(inherits(p4, "ggplot") || inherits(p4, "ggmatrix"))

  # 3) Train model (PCA + kNN)
  model <- train_model(
    feature_embedding = trainset,
    dr    = "pca",
    dr_k  = 20,
    model = "knn"
  )

  expect_s3_class(model, "histolearn")
  expect_true(all(c("dr_model", "model", "train_pred", "method", "dr_dim") %in% names(model)))
  expect_true(is.numeric(model$dr_dim))
  expect_gt(model$dr_dim, 0)

  # 4) Evaluate model
  eval_res <- evaluate_model(
    trained_model = model,
    test_data     = testset
  )

  expect_type(eval_res, "list")
  expect_true(all(c("conf_matrix", "metric") %in% names(eval_res)))

  # confusion matrix should be a ggplot (or ggmatrix, depending on impl)
  expect_true(inherits(eval_res$conf_matrix, "ggplot") || inherits(eval_res$conf_matrix, "ggmatrix"))

  # metric should contain (at least) accuracy between 0 and 1
  metric <- eval_res$metric
  expect_true(is.list(metric) || is.numeric(metric) || is.data.frame(metric))

  if (is.list(metric) && "accuracy" %in% names(metric)) {
    acc <- metric$accuracy
    expect_true(is.numeric(acc))
    expect_gte(acc, 0)
    expect_lte(acc, 1)
  }
})
