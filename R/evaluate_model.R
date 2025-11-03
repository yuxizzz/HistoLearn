#' Evaluate Model Performance
#'
#' Evaluate a trained classification model on a test dataset. The function applies
#' the model’s dimensionality-reduction transformation to the test features,
#' generates predictions, computes overall accuracy, and visualizes the confusion
#' matrix using \pkg{ggplot2}.
#'
#' @param trained_model A fitted model object returned by \code{\link{train_model}}.
#'   Must include components \code{dr_model}, \code{model}, and \code{dr_dim}.
#' @param test_data An object of class \code{"histofeature"} created by
#'   \code{\link{load_embeddings}}, containing test features and labels.
#'
#' @return
#' A list containing:
#' \itemize{
#'   \item \code{conf_matrix} — a \code{ggplot} object showing the confusion matrix.
#'   \item \code{metric} — a list with evaluation metrics (currently accuracy).
#' }
#'
#' @examples
#' data(train_embeddings)
#' data(train_labels)
#' data(test_embeddings)
#' data(test_labels)
#' train_set <- load_embeddings(feature=train_embeddings, label=train_labels)
#' test_set <- load_embeddings(feature=test_embeddings, label=test_labels)
#' model <- train_model(feature_embedding=train_set,
#'                       dr="pca", dr_k=20, model = "knn")
#' evaluate_model(model, test_set)
#'
#' @references
#'
#' Kuhn, M. (2008). Building Predictive Models in R Using the caret
#' Package. Journal of Statistical Software, 28(5), 1–26.
#' https://doi.org/10.18637/jss.v028.i05
#'
#' H. Wickham. ggplot2: Elegant Graphics for Data Analysis.
#' Springer-Verlag New York, 2016.
#'
#' @export
#' @import caret ggplot2
evaluate_model <- function(trained_model, test_data) {

  X_test_dr <- NULL
  X_test <- NULL
  X_test <- predict(trained_model$dr_model, test_data$feature)
  k <- trained_model$dr_dim
  X_test_dr <- X_test[, 1:k, drop = FALSE]
  predictions <- predict(trained_model$model,
                         X_test_dr,
                         type = "class")
  cm <- caret::confusionMatrix(predictions, test_data$label)
  cm_df <- as.data.frame(cm$table)
  metrics <- list(
    Accuracy = cm$overall["Accuracy"]
  )

  p <- ggplot2::ggplot(cm_df, aes(x = Prediction, y = Reference, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "white", size = 5) +
    scale_fill_gradient(low = "skyblue", high = "darkblue") +
    labs(
      title = "Confusion Matrix",
      x = "Predicted Label",
      y = "True Label"
    ) +
    theme_minimal()
  return(list(conf_matrix=p, metric=metrics))
}
#[END]
