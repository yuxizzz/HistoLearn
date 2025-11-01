#' Train a classifier on embeddings
#'
#' @param feature_embedding input
#' @param dr now only "pca"
#' @param dr_k integer, target dimension for reduction
#' @param model now only knn
#' @return a list with fitted model, reduced features, and the DR info
#' @examples
#' data(train_embeddings)
#' data(train_labels)
#' train_set <- load_embeddings(feature=train_embeddings, label=train_labels)
#' model <- train_model(feature_embedding=train_set,
#'                       dr="pca", dr_k=20, model = "knn")
#' @export
#' @import caret stats ggplot2 lattice
train_model <- function(feature_embedding, dr = "pca", dr_k = 20, model = "knn") {
  dr <- match.arg(dr)
  model <- match.arg(model)

  X <- feature_embedding$feature
  y <- feature_embedding$label

  dr_out <- reduce_dim(X, method = dr, k = dr_k)
  X_dr <- dr_out$X_red

  # Model training
  if (model == "knn") {
    model_out <- train_knn(X_dr, y)
    fitted_model <- model_out$best_model
  } else {
    stop("Unsupported model type: ", model)
  }

  # Predictions
  train_predictions <- predict(fitted_model, X_dr, type = "class")

  # Return results
  list(
    dr_model = dr_out$model,
    model = fitted_model,
    train_pred = train_predictions,
    method = c(dr, model)
  )
}

#' Dimension reduction helper
#' @param X numeric matrix
#' @param method currently only "pca"
#' @param k integer; number of components to keep
#' @return list with reduced matrix and model
#' @import stats
reduce_dim <- function(X, method = "pca", k = 20) {
  X_red <- X
  if (method == "pca") {
    pca <- stats::prcomp(X, center = TRUE, scale. = TRUE)
    k   <- min(k, ncol(pca$x))
    X_red <- pca$x[, 1:k, drop = FALSE]
    return(list(model = pca, X_red = X_red))
  } else{
    stop("Unsupported dimension reduction method: ", method)
  }
}


#' KNN training helper
#' @param X matrix or data.frame of predictors
#' @param y factor of class labels
#' @return list with best tuned model and fitted knn3 object
#' @keywords internal
#' @import caret
train_knn <- function(X, y) {
  stopifnot(nrow(X) == length(y))
  knn_fit <- caret::train(
    x = X,
    y = as.factor(y),
    method = "knn",
    tuneLength = 10,
    trControl = caret::trainControl(method = "cv"),
    preProcess = c("center", "scale")
  )
  best_model <- caret::knn3(
    x = X,
    y = as.factor(y),
    k = knn_fit$bestTune$k
  )
  return(list(best_model = best_model, best_k = knn_fit$bestTune$k))
}

#[END]
