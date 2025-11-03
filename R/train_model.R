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
#'
#' @references
#' Chen, R. J., Ding, T., Lu, M. Y., Williamson, D. F. K., Jaume, G.,
#' Song, A. H., Chen, B., Zhang, A., Shao, D., Shaban, M., Williams, M.,
#' Oldenburg, L., Weishaupt, L. L., Wang, J. J., Vaidya, A., Le, L. P., Gerber,
#' G., Sahai, S., Williams, W., & Mahmood, F. (2024). Towards a general-purpose
#' foundation model for computational pathology. Nature Medicine, 30(3), 850–862.
#' https://doi.org/10.1038/s41591-024-02857-3
#'
#' Kuhn, M. (2008). Building Predictive Models in R Using the caret
#' Package. Journal of Statistical Software, 28(5), 1–26.
#' https://doi.org/10.18637/jss.v028.i05
#'
#' R Core Team (2025). _R: A Language and Environment for
#' Statistical Computing_. R Foundation for Statistical Computing,
#' Vienna, Austria. <https://www.R-project.org/>.
#'
#' H. Wickham. ggplot2: Elegant Graphics for Data Analysis.
#' Springer-Verlag New York, 2016.
#'
#' Sarkar D (2008). _Lattice: Multivariate Data Visualization with
#' R_. Springer, New York. ISBN 978-0-387-75968-5,
#' <http://lmdvr.r-forge.r-project.org>.
#'
#' @note
#' Documentation and code debugging processes were refined with the assistance
#' of ChatGPT-5 to improve accuracy and clarity.
#'
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
#' Performs dimensionality reduction on a numeric feature matrix.
#' Currently, only Principal Component Analysis (PCA) is implemented.
#'
#' @param X A numeric matrix where rows represent samples and columns represent features.
#' @param method A character string specifying the dimensionality reduction method.
#'   Currently only \code{"pca"} is supported.
#' @param k An integer specifying the number of components to retain.
#'
#' @return
#' A list with the following elements:
#' \itemize{
#'   \item \code{model} — the fitted PCA model.
#'   \item \code{X_red} — the matrix of reduced-dimension features.
#' }
#' @references
#' R Core Team (2025). _R: A Language and Environment for
#' Statistical Computing_. R Foundation for Statistical Computing,
#' Vienna, Austria. <https://www.R-project.org/>.
#'
#' @note
#' Documentation and code debugging processes were refined with the assistance
#' of ChatGPT-5 to improve accuracy and clarity.
#'
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

#' Train a k-Nearest Neighbors (k-NN) Classifier
#'
#' Internal helper function that trains and tunes a k-Nearest Neighbors (k-NN)
#' classification model using the \pkg{caret} package.
#'
#' @param X A matrix or data frame of predictor variables.
#' @param y A factor vector of class labels corresponding to the rows of \code{X}.
#'
#' @return
#' A fitted \code{knn3} model using the optimal value of \code{k} determined
#' via cross-validation.
#'
#' @references
#' Kuhn, M. (2008). Building Predictive Models in R Using the caret
#' Package. Journal of Statistical Software, 28(5), 1–26.
#' https://doi.org/10.18637/jss.v028.i05
#'
#' @note
#' Documentation and code debugging processes were refined with the assistance
#' of ChatGPT-5 to improve accuracy and clarity.
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
