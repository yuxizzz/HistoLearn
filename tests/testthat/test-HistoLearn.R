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
  expect_true(all(
    c("dr_model", "model", "train_cm", "train_acc", "method", "dr_dim")
    %in% names(model)
  ))
  expect_true(is.numeric(model$dr_dim))
  expect_gt(model$dr_dim, 0)
  expect_s3_class(model$train_cm, "confusionMatrix")
  expect_true(is.numeric(model$train_acc))
  expect_true(model$train_acc >= 0 && model$train_acc <= 1)

  # 4) Evaluate model
  eval_res <- evaluate_model(
    trained_model = model,
    test_data     = testset
  )

  expect_type(eval_res, "list")
  expect_true(all(
    c("train_conf_matrix", "train_metric", "test_conf_matrix", "test_metric")
    %in% names(eval_res)
  ))

  # confusion matrices should be ggplot objects
  expect_s3_class(eval_res$train_conf_matrix, "ggplot")
  expect_s3_class(eval_res$test_conf_matrix,  "ggplot")

  # metrics should be numeric accuracies in [0, 1]
  expect_true(is.numeric(eval_res$train_metric))
  expect_true(length(eval_res$train_metric) == 1)
  expect_true(eval_res$train_metric >= 0 && eval_res$train_metric <= 1)

  expect_true(is.numeric(eval_res$test_metric))
  expect_true(length(eval_res$test_metric) == 1)
  expect_true(eval_res$test_metric >= 0 && eval_res$test_metric <= 1)

  # 5) Train model (PCA + logistic regression)
  model_logit <- train_model(
    feature_embedding = trainset,
    dr    = "pca",
    dr_k  = 20,
    model = "logistic"
  )

  expect_s3_class(model_logit, "histolearn")
  expect_true(all(
    c("dr_model", "model", "train_cm", "train_acc", "method", "dr_dim") %in%
      names(model_logit)
  ))
  expect_true(is.numeric(model_logit$dr_dim))
  expect_gt(model_logit$dr_dim, 0)
  expect_s3_class(model_logit$train_cm, "confusionMatrix")
  expect_true(is.numeric(model_logit$train_acc))
  expect_true(model_logit$train_acc >= 0 && model_logit$train_acc <= 1)
  expect_identical(model_logit$method, c("pca", "logistic"))
  expect_s3_class(model_logit$model, "train")
  expect_equal(model_logit$model$method, "multinom")

  # 6) Evaluate model (logistic)
  eval_logit <- evaluate_model(
    trained_model = model_logit,
    test_data     = testset
  )

  expect_type(eval_logit, "list")
  expect_true(all(
    c("train_conf_matrix", "train_metric", "test_conf_matrix", "test_metric")
    %in% names(eval_logit)
  ))

  expect_s3_class(eval_logit$train_conf_matrix, "ggplot")
  expect_s3_class(eval_logit$test_conf_matrix,  "ggplot")

  expect_true(is.numeric(eval_logit$train_metric))
  expect_true(length(eval_logit$train_metric) == 1)
  expect_true(eval_logit$train_metric >= 0 && eval_logit$train_metric <= 1)

  expect_true(is.numeric(eval_logit$test_metric))
  expect_true(length(eval_logit$test_metric) == 1)
  expect_true(eval_logit$test_metric >= 0 && eval_logit$test_metric <= 1)
})
# [END]
