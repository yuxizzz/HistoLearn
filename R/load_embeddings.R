#' Load in Histological Features
#'
#' A function that load in histological features extracted from any
#' pathology foundation model. Optionally include labels
#' given log-likelihood, number of clusters, dimensionality of the
#' dataset and the number of observations.
#'
#' @param feature The matrix/data.frame; rows are samples
#' @param label The vector or 1-col data.frame/matrix of labels (or NULL)
#' @return Returns an object of class \code{"histofeature"}
#' \itemize{
#'   \item feature - A data.frame of features with columns named "1","2",...
#'   \item label - A factor of labels or \code{NULL}
#' }
#'
#' @examples
#' data(train_embeddings)
#' data(train_labels)
#' train_set <- load_embeddings(feature=train_embeddings, label=train_labels)
#'
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
load_embeddings <- function(feature, label = NULL) {
  result <- as.data.frame(feature)
  colnames(result) <- seq_len(ncol(result))
  feature_embedding <- list(feature = result,
                            label = NULL)
  if (!is.null(label)){
    if (is.data.frame(label) && ncol(label) == 1L) label <- label[[1]]
    if (is.matrix(label) && ncol(label) == 1L) label <- label[, 1]
    labels_fac  <- as.factor(label)
    if (nrow(feature) != length(labels_fac)) {
      stop("feature and label dimension doesn't match", call.=TRUE)
    }
    feature_embedding$label <- labels_fac
  }
  class(feature_embedding) <- "histofeature"
  return(feature_embedding)
}
#[END]
