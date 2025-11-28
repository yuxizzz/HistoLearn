#' Train a classifier on embeddings
#'
#' @description
#' Fits a supervised classification model using reduced-dimensional embeddings.
#' Dimensionality reduction (currently PCA) is applied to the input features
#' before training the specified classifier. Supported classifiers are
#' k-nearest neighbors (\code{"knn"}) and multinomial logistic regression
#' (\code{"logistic"}) with cross-validation and standardization (centering
#' and scaling).
#'
#' @param feature_embedding An object of class \code{"histofeature"} as returned
#'   by \code{\link{load_embeddings}}.
#' @param dr A character string specifying the dimensionality reduction method.
#'   Currently only \code{"pca"} is supported.
#' @param dr_k An integer specifying the target reduced dimension (number of
#'   components to retain).
#' @param model A character string specifying the classifier type. Supported
#'   options are \code{"knn"} for k-nearest neighbors and \code{"logistic"} for
#'   logistic regression.
#'
#' @return
#' An object of class \code{"histolearn"}, which is a list with elements:
#' \itemize{
#'   \item \code{dr_model} — the fitted dimensionality reduction model (e.g., PCA).
#'   \item \code{model} — the fitted classification model (a
#'         \code{\link[caret]{train}} object).
#'   \item \code{train_cm} — the confusion matrix obtained by comparing model
#'         predictions on the training data to the true labels. Returned as a
#'         \code{\link[caret]{confusionMatrix}} object.
#'   \item \code{train_acc} — numeric scalar giving the training accuracy
#'         (extracted from \code{train_cm$overall["Accuracy"]}).
#'   \item \code{method} — character vector of length two indicating the
#'         reduction method and classifier type.
#'   \item \code{dr_dim} — integer specifying the reduced dimension.
#' }
#'
#' @examples
#' \dontrun{
#' data(train_embeddings)
#' data(train_labels)
#' train_set <- load_embeddings(feature = train_embeddings,
#'                              label   = train_labels)
#' model <- train_model(feature_embedding = train_set,
#'                      dr = "pca",
#'                      dr_k = 20,
#'                      model = "knn")
#' }
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
#' Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth Edition.
#' Springer, New York. ISBN 0-387-95457-0
#'
#' OpenAI. (2025). ChatGPT (GPT-5.1, February 2025 version)
#' (Large language model). <https://chat.openai.com/>
#'
#'
#' @note
#' Documentation and code debugging processes were refined with the assistance
#' of ChatGPT-5 to improve accuracy and clarity.
#'
#' @export
#' @import caret stats ggplot2 lattice
train_model <- function(feature_embedding,
                        dr = "pca",
                        dr_k  = 20,
                        model = c("knn", "logistic")) {
  # Validate input type
  if (!inherits(feature_embedding, "histofeature")) {
    stop("invalid input", call. = TRUE)
  }

  # Extract feature matrix and labels
  X <- as.matrix(feature_embedding$feature)
  y <- feature_embedding$label

  # Apply dimensionality reduction (currently PCA)
  dr_out <- reduce_dim(X, method = dr, k = dr_k)
  X_dr <- dr_out$X_red

  # Train the requested classifier
  if (model == "knn") {
    fitted_model <- train_knn(X_dr, y)
  } else if (model == "logistic") {
    fitted_model <- train_logistic(X_dr, y)
  } else {
    stop("Unsupported model type: ", model)
  }

  # Predict on training data
  preds <- predict(fitted_model, X_dr)

  # Compute confusion matrix for training performance
  cm <- caret::confusionMatrix(preds, y)

  # Construct result object containing DR model, classifier, and metrics
  res_model <- list(
    dr_model = dr_out$model,
    model = fitted_model,
    train_cm  = cm,
    train_acc = unname(cm$overall["Accuracy"]),
    method = c(dr, model),
    dr_dim = dr_k
  )

  # Assign S3 class
  class(res_model) <- "histolearn"

  return(res_model)
}

#' Dimension Reduction Helper
#'
#' A helper function that performs dimensionality reduction on a numeric feature matrix.
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
#' OpenAI. (2025). ChatGPT (GPT-5.1, February 2025 version)
#' (Large language model). <https://chat.openai.com/>
#'
#' R Core Team (2025). _R: A Language and Environment for
#' Statistical Computing_. R Foundation for Statistical Computing,
#' Vienna, Austria. <https://www.R-project.org/>.
#'
#' @note
#' Documentation and code debugging processes were refined with the assistance
#' of ChatGPT-5 to improve accuracy and clarity.
#'
#' @keywords internal
#' @import stats
reduce_dim <- function(X, method = "pca", k = 20) {

  # Initialize reduced matrix as original data
  X_red <- X

  # Validate requested dimension k
  if (k > ncol(X_red)) {
    stop("invalid dimension", call. = TRUE)
  }

  # Perform PCA if selected method is supported
  if (method == "pca") {
    # Fit PCA model (center + scale for standardization)
    pca <- stats::prcomp(X, center = TRUE, scale. = TRUE)
    # Ensure k does not exceed available components
    k <- min(k, ncol(pca$x))
    # Extract reduced feature matrix with k components
    X_red <- pca$x[ , 1:k, drop = FALSE]
    # Return DR model and transformed matrix
    return(list(model = pca, X_red = X_red))
  } else {
    stop("Unsupported dimension reduction method: ", method)
  }
}

#' Train a k-Nearest Neighbors (k-NN) Classifier
#'
#' Internal helper function that trains and tunes a k-nearest neighbors (k-NN)
#' classification model using the \pkg{caret} package. The predictors are
#' centered and scaled, and \code{k} is selected via cross-validation.
#'
#' @param X A matrix or data frame of predictor variables (samples in rows,
#'   features in columns).
#' @param y A factor or character vector of class labels corresponding to the
#'   rows of \code{X}. Coerced internally to a factor.
#'
#' @return
#' A fitted \code{\link[caret]{train}} object with \code{method = "knn"}, which
#' encapsulates the cross-validated k-NN model and tuning results.
#'
#' @references
#' Kuhn, M. (2008). Building Predictive Models in R Using the \pkg{caret}
#' Package. \emph{Journal of Statistical Software}, 28(5), 1–26.
#' https://doi.org/10.18637/jss.v028.i05
#'
#' OpenAI. (2025). ChatGPT (GPT-5.1, February 2025 version)
#' (Large language model). \url{https://chat.openai.com/}
#'
#' @note
#' Documentation and code debugging processes were refined with the assistance
#' of ChatGPT-5 to improve accuracy and clarity.
#'
#' @keywords internal
#' @import caret
train_knn <- function(X, y) {

  # Ensure predictors are in data.frame format
  X <- as.data.frame(X)

  # Ensure columns have valid, non-empty names
  if (is.null(colnames(X)) || any(!nzchar(colnames(X)))) {
    colnames(X) <- paste0("x", seq_len(ncol(X)))
  }

  # Fit k-NN classifier using caret with CV, centering, and scaling
  knn_fit <- caret::train(
    x = X,
    y = as.factor(y),
    method = "knn",
    tuneLength = 10,
    trControl = caret::trainControl(method = "cv"),
    preProcess = c("center", "scale")
  )
  return(knn_fit)
}


#' Train a Multinomial Logistic Regression Classifier
#'
#' Internal helper function that trains a multinomial logistic regression
#' classification model using the \pkg{caret} interface to
#' \code{\link[nnet]{multinom}} (via \code{method = "multinom"}). Predictors
#' are centered and scaled prior to fitting, and performance is estimated
#' using cross-validation.
#'
#' @param X A matrix or data frame of predictor variables (samples in rows,
#'   features in columns).
#' @param y A factor or character vector of class labels corresponding to the
#'   rows of \code{X}. Coerced internally to a factor.
#'
#' @return
#' A fitted \code{\link[caret]{train}} object with \code{method = "multinom"},
#' whose \code{finalModel} component is an \code{\link[nnet]{multinom}} object.
#'
#' @references
#'
#' Kuhn, M. (2008). Building Predictive Models in R Using the \pkg{caret}
#' Package. \emph{Journal of Statistical Software}, 28(5), 1–26.
#' https://doi.org/10.18637/jss.v028.i05
#'
#' OpenAI. (2025). ChatGPT (GPT-5.1, February 2025 version)
#' (Large language model). \url{https://chat.openai.com/}
#'
#' R Core Team (2025). \emph{R: A Language and Environment for Statistical
#' Computing}. R Foundation for Statistical Computing, Vienna, Austria.
#' https://www.R-project.org/
#'
#' Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with S.
#' Fourth Edition. Springer, New York. ISBN 0-387-95457-0
#'
#' @note
#' Documentation and code debugging processes were refined with the assistance
#' of ChatGPT-5 to improve accuracy and clarity.
#'
#' @keywords internal
#' @import caret nnet
train_logistic <- function(X, y) {

  # Ensure predictors are in data.frame format
  X <- as.data.frame(X)

  # Ensure columns have valid, non-empty names
  if (is.null(colnames(X)) || any(!nzchar(colnames(X)))) {
    colnames(X) <- paste0("x", seq_len(ncol(X)))
  }

  # Train multinomial logistic regression classifier via caret (method = "multinom")
  lr_fit <- caret::train(
    x = X,
    y = y,
    method = "multinom",
    trControl = caret::trainControl(method = "cv"),
    preProcess = c("center", "scale")
  )
  return(lr_fit)
}
# [END]
