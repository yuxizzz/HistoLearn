set.seed(42)
X_train <- rbind(
  matrix(rnorm(100, mean = 0, sd = 1),  nrow = 20, ncol = 5),
  matrix(rnorm(100, mean = 3, sd = 1),  nrow = 20, ncol = 5)
)
y_train <- factor(c(rep("A", 20), rep("B", 20)))
trainset <- load_embeddings(X_train, y_train)
X_test <- rbind(
  matrix(rnorm(50,  mean = 0, sd = 1), nrow = 10, ncol = 5),
  matrix(rnorm(50,  mean = 3, sd = 1), nrow = 10, ncol = 5)
)
y_test <- factor(c(rep("A", 10), rep("B", 10)))
testset <- load_embeddings(X_test, y_test)
model <- train_model(trainset, dr = "pca", dr_k = 2, model = "knn")

test_that("evaluate_model returns confusion matrix and metrics", {
  output <- evaluate_model(trained_model=model, test_data=testset)
  expect_type(output, "list")
  expect_true(all(c("conf_matrix", "metric") %in% names(output)))
  expect_s3_class(output$conf_matrix, "ggplot")
  expect_true(is.list(output$metric))
  expect_true(all(c("Accuracy") %in% names(output$metric)))
  expect_true(output$metric["Accuracy"] >= 0 && output$metric["Accuracy"] <= 1)
})
