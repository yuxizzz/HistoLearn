#' Load Histological Feature Embeddings
#'
#' Load histology feature embeddings (rows = samples, columns = features)
#' extracted from a pathology foundation model, and optionally attach sample
#' labels. Feature columns are renamed to consecutive integers starting at 1.
#'
#' @param feature A numeric \code{matrix} or \code{data.frame} of features where
#'   rows are samples/observations and columns are feature dimensions.
#' @param label Optional labels for each sample. May be a vector, factor, or a
#'   1-column \code{data.frame}/\code{matrix}. If supplied, its length must equal
#'   \code{nrow(feature)}. Converted internally to a \code{factor}.
#'
#' @return An object of class \code{"histofeature"} with:
#' \itemize{
#'   \item \code{feature}: a \code{data.frame} of features with columns named
#'         \code{"1"}, \code{"2"}, \code{"3"}, \ldots
#'   \item \code{label}: a \code{factor} of labels or \code{NULL}
#' }
#'
#' @examples
#' data(train_embeddings)
#' data(train_labels)
#' train_set <- load_embeddings(feature=train_embeddings, label=train_labels)
#'
#' @note
#' Documentation and debugging processes were refined with the assistance
#' of ChatGPT-5 to improve accuracy and clarity.
#'
#' @export
load_embeddings <- function(feature, label = NULL) {
  result <- as.data.frame(feature)
  if (anyNA(result)) {
    stop("feature matrix contains NA values.", call. = TRUE)
  }
  if (any(is.infinite(as.matrix(result)))) {
    stop("feature matrix contains Inf or -Inf values.", call. = TRUE)
  }

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
