# Purpose: Load histology feature embeddings and attach labels to create a
#          standardized "histofeature" object for downstream HistoLearn analysis.
# Author: Yuxi Zhu
# Date: 2025-11-27
# Version: 0.1.0
# Bugs and Issues: None known

#' Load Histological Feature Embeddings
#'
#' Load histology feature embeddings (rows = samples, columns = features)
#' extracted from a pathology foundation model, and optionally attach sample
#' labels. Feature columns are renamed to consecutive integers starting at 1.
#'
#' @param feature A numeric \code{matrix} or \code{data.frame} of features where
#'   rows are samples/observations and columns are feature dimensions.
#' @param label The labels for each sample. May be a vector, factor, or a
#'   1-column \code{data.frame}/\code{matrix}. If supplied, its length must equal
#'   \code{nrow(feature)}. Converted internally to a \code{factor}.
#'
#' @return An object of class \code{"histofeature"} with:
#' \itemize{
#'   \item \code{feature}: a \code{data.frame} of features with columns named
#'         \code{"1"}, \code{"2"}, \code{"3"}, \ldots
#'   \item \code{label}: a \code{factor} of labels
#' }
#'
#' @examples
#' \dontrun{
#' data(train_embeddings)
#' data(train_labels)
#' train_set <- load_embeddings(feature = train_embeddings,
#'                              label = train_labels)
#'}
#' @references
#' OpenAI. (2025). ChatGPT (GPT-5.1, February 2025 version)
#' (Large language model). <https://chat.openai.com/>
#'
#' @note
#' Documentation and debugging processes were refined with the assistance
#' of ChatGPT-5 to improve accuracy and clarity.
#'
#' @export
load_embeddings <- function(feature, label) {
  # Coerce feature to data.frame for downstream consistency.
  result <- as.data.frame(feature)

  # error checking for input
  if (nrow(result) == 0L || ncol(result) == 0L) {
    stop("feature must have at least one row and one column.", call. = TRUE)
  }
  if (! all(vapply(result, is.numeric, logical(1)))) {
    stop("All columns in 'feature' must be numeric.", call. = TRUE)
  }
  if (anyNA(result)) {
    stop("feature matrix contains NA values.", call. = TRUE)
  }
  if (any(is.infinite(as.matrix(result)))) {
    stop("feature matrix contains Inf or -Inf values.", call. = TRUE)
  }
  if (is.null(label)) {
    stop("Embedding labels are not supplied. Please provide 'label' for the pipeline.",
         call. = TRUE)
  }

  # check label is 1D
  if (is.data.frame(label)) {
    if (ncol(label) != 1L) {
      stop("If 'label' is a data.frame, it must have exactly one column.", call. = TRUE)
    }
    label <- label[[1]]
  } else if (is.matrix(label)) {
    # If label is a matrix: must have exactly one column
    if (ncol(label) != 1L) {
      stop("If 'label' is a matrix, it must have exactly one column.", call. = TRUE)
    }
    label <- label[ , 1]
  } else if (! (is.vector(label) || is.factor(label))) {
    # Anything else is not supported
    stop("'label' must be a vector, factor, 1-column matrix, or 1-column data.frame.",
         call. = TRUE)
  }

  # Convert labels to factor.
  labels_fac <- as.factor(label)

  # Check that number of labels matches number of samples.
  if (nrow(result) != length(labels_fac)) {
    stop("feature and label dimensions do not match.", call. = TRUE)
  }

  # Rename columns to consecutive integers as character strings: "1", "2", ...
  colnames(result) <- as.character(seq_len(ncol(result)))

  # construct histofeature object
  feature_embedding <- list(
    feature = result,
    label = labels_fac
  )
  class(feature_embedding) <- "histofeature"

  return(feature_embedding)
}
# [END]
