#' Train a classifier on embeddings
#'
#' @description
#' Fits a supervised classification model using reduced-dimensional embeddings.
#' Dimensionality reduction (currently PCA) is applied to the input features before
#' training the specified classifier (currently K-Nearest Neighbors).
#'
#' @param feature_embedding An object of class \code{"histofeature"}
#' @param dr A character string specifying the dimensionality reduction method.
#'   Currently only \code{"pca"} is supported.
#' @param dr_k An integer specifying the target reduced dimension
#' @param model A character string specifying the classifier type, Currently
#' only K-Nearest Neighbors (knn) is supported.
#' @return
#' A list of class \code{"histolearn"} with elements:
#' \itemize{
#'   \item dr_model — the fitted dimensionality reduction model.
#'   \item model — the fitted classification model.
#'   \item train_pred — model predictions on the training data.
#'   \item method — character vector of the reduction and model type.
#'   \item dr_dim — integer specifying the reduced dimension.
#' }
#' @examples
#' data(train_embeddings)
#' data(train_labels)
#' train_set <- load_embeddings(feature=train_embeddings, label=train_labels)
#' model <- train_model(feature_embedding=train_set,
#'                       dr="pca", dr_k=20, model = "knn")
#' @export
#' @import caret stats ggplot2 lattice
train_model <- function(feature_embedding, dr = "pca", dr_k = 20, model = "knn") {
  if (!inherits(feature_embedding, "histofeature")) {
    stop("invalid input", call.=TRUE)
  }

  X <- as.matrix(feature_embedding$feature)
  y <- feature_embedding$label

  if (is.null(y)) {
    stop("no label found. Labels are required for supervised learning", call.=TRUE)
  }

  dr_out <- reduce_dim(X, method = dr, k = dr_k)
  X_dr <- dr_out$X_red

  if (model == "knn") {
    fitted_model <- train_knn(X_dr, y)
  } else {
    stop("Unsupported model type: ", model)
  }

  train_predictions <- predict(fitted_model, X_dr, type = "class")

  # Return results
  res_model <- list(
    dr_model = dr_out$model,
    model = fitted_model,
    train_pred = train_predictions,
    method = c(dr, model),
    dr_dim = dr_k
  )
  class(res_model) <- "histolearn"
  return(res_model)
}

#' Dimension Reduction Helper
#'
#' @description
#' Performs dimensionality reduction on a numeric feature matrix.
#' Currently, only Principal Component Analysis (PCA) is implemented.
#'
#' @param X numeric matrix
#' @param method currently only "pca"
#' @param k integer; number of components to keep
#' @return A list containing:
#' \itemize{
#'   \item model — the fitted PCA model.
#'   \item X_red — the matrix of reduced features.
#' }
#' @import stats
reduce_dim <- function(X, method = "pca", k = 20) {
  X_red <- X
  if (k > ncol(X_red)) {
    stop("invalid dimension", call.=TRUE)
  }
  if (method == "pca") {
    pca <- stats::prcomp(X, center = TRUE, scale. = TRUE)
    k   <- min(k, ncol(pca$x))
    X_red <- pca$x[, 1:k, drop = FALSE]
    return(list(model = pca, X_red = X_red))
  } else{
    stop("Unsupported dimension reduction method: ", method)
  }
}
#' Train a K-Nearest Neighbors (KNN) Classifier
#'
#' @description
#' Internal helper function that trains and tunes a KNN model.
#'
#' @param X Matrix of predictor variables.
#' @param y Factor of class labels corresponding to the rows of \code{X}.
#'
#' @return The best fitted model using the best K.
#'
#' @import caret
train_knn <- function(X, y) {

  X <- as.data.frame(X)
  if (is.null(colnames(X)) || any(!nzchar(colnames(X)))) {
    colnames(X) <- paste0("x", seq_len(ncol(X)))
  }
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
  return(best_model)
}

#[END]
