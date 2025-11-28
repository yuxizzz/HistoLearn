#' Evaluate Model Performance
#'
#' Evaluate a trained classification model on a test dataset. The function
#' applies the trained dimensionality-reduction transformation to the test
#' features, generates predictions using the fitted classifier, computes overall
#' accuracy, and visualizes the training and test confusion matrices using
#' \pkg{ggplot2}.
#'
#' @param trained_model A fitted model object returned by
#'   \code{\link{train_model}}. Must include components \code{dr_model},
#'   \code{model}, \code{dr_dim}, \code{train_cm}, and \code{train_acc}.
#' @param test_data An object of class \code{"histofeature"} created by
#'   \code{\link{load_embeddings}}, containing test features and labels.
#'
#' @return
#' A list with the following elements:
#' \itemize{
#'   \item \code{train_conf_matrix} — a \code{ggplot} object visualizing the
#'         training confusion matrix.
#'   \item \code{train_metric} — numeric training accuracy.
#'   \item \code{test_conf_matrix} — a \code{ggplot} object visualizing the
#'         test confusion matrix.
#'   \item \code{test_metric} — numeric test accuracy.
#' }
#'
#' @examples
#' \dontrun{
#' data(train_embeddings)
#' data(train_labels)
#' data(test_embeddings)
#' data(test_labels)
#' train_set <- load_embeddings(feature = train_embeddings, label = train_labels)
#' test_set <- load_embeddings(feature = test_embeddings, label = test_labels)
#' model <- train_model(feature_embedding = train_set,
#'                       dr = "pca", dr_k = 20, model = "knn")
#' evaluate_model(model, test_set)
#'}
#' @references
#' H. Wickham. ggplot2: Elegant Graphics for Data Analysis.
#' Springer-Verlag New York, 2016.
#'
#' Kuhn, M. (2008). Building Predictive Models in R Using the caret
#' Package. Journal of Statistical Software, 28(5), 1–26.
#' https://doi.org/10.18637/jss.v028.i05
#'
#' OpenAI. (2025). ChatGPT (GPT-5.1, February 2025 version)
#' (Large language model). <https://chat.openai.com/>
#'
#' @export
#' @import caret ggplot2
evaluate_model <- function(trained_model, test_data) {
  # check inputs
  if (!inherits(test_data, "histofeature")) {
    stop("`test_data` must be a 'histofeature' object.", call. = TRUE)
  }
  if (!inherits(trained_model, "histolearn")) {
    stop("`trained_model` must be a 'histolearn' object.", call. = TRUE)
  }

  # Apply the trained dimensionality reduction model to test features
  X_test <- predict(trained_model$dr_model, test_data$feature)

  # Retrieve the reduced dimension used during training
  k <- trained_model$dr_dim

  # Subset the projected test features to the first k components
  X_test_dr <- X_test[ , 1:k, drop = FALSE]

  # Generate predictions on the reduced test data
  predictions <- predict(trained_model$model, X_test_dr)

  # Compute confusion matrix and extract accuracy for test set
  cm_test <- caret::confusionMatrix(predictions, test_data$label)
  cm_test_df <- as.data.frame(cm_test$table)
  acc_test <- unname(cm_test$overall["Accuracy"])

  # Retrieve the stored training confusion matrix and accuracy
  cm_train <- trained_model$train_cm
  acc_train <- unname(trained_model$train_acc)
  cm_train_df <- as.data.frame(cm_train$table)

  # Plot test confusion matrix as a heatmap
  p <- ggplot2::ggplot(
    cm_test_df,
    aes(
      x = cm_test_df$Prediction,
      y = cm_test_df$Reference,
      fill = cm_test_df$Freq
    )
  ) +
    geom_tile(color = "white") +
    geom_text(aes(label = cm_test_df$Freq), color = "white", size = 5) +
    scale_fill_gradient(low = "skyblue", high = "darkblue") +
    labs(
      title = "Test Confusion Matrix",
      x = "Predicted Label",
      y = "True Label"
    ) +
    theme_minimal()

  # Plot training confusion matrix for comparison
  p_train <- ggplot2::ggplot(
    cm_train_df,
    aes(
      x = cm_train_df$Prediction,
      y = cm_train_df$Reference,
      fill = cm_train_df$Freq
    )
  ) +
    geom_tile(color = "white") +
    geom_text(aes(label = cm_train_df$Freq), color = "white", size = 5) +
    scale_fill_gradient(low = "skyblue", high = "darkblue") +
    labs(
      title = "Train Confusion Matrix",
      x = "Predicted Label",
      y = "True Label"
    ) +
    theme_minimal()
  return(list(train_conf_matrix=p_train, train_metric=acc_train,
              test_conf_matrix=p, test_metric=acc_test))
}
# [END]
