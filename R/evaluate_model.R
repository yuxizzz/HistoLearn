#' Evaluate model
#'
#' A function that calculates information criteria, BIC (Bayesian
#' Information Criterion) and AIC (Akaike Information Criterion)
#' given log-likelihood, number of clusters, dimensionality of the
#' dataset and the number of observations.
#'
#' @param trained_model A negative value of class numeric indicating
#'   the log-likelihood.
#' @param test_data A positive integer indicating the number of clusters.
#'
#' @return a list
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
#' @references
#'
#' Schwarz, G. (1978). Estimating the dimension of a model. \emph{The Annals of Statistics}
#' 6.
#'
#' Akaike, H. (1973). Information theory and an extension of the maximum likelihood
#' principle. In \emph{Second International Symposium on Information Theory}, New York, NY,
#' USA, pp. 267–281. Springer Verlag.
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
