# R/data.R

#' Training embeddings (example data)
#'
#' @format A data.frame with \eqn{n} rows (samples) and \eqn{p} columns (features).
#' @usage data(train_embeddings)
#' @docType data
#' @keywords datasets
"train_embeddings"

#' Training labels (example data)
#' @format A factor or character vector of length \eqn{n}.
#' @usage data(train_labels)
#' @docType data
#' @keywords datasets
"train_labels"

#' Test embeddings (example data)
#' @format A data.frame like \code{train_embeddings}.
#' @usage data(test_embeddings)
#' @docType data
#' @keywords datasets
"test_embeddings"

#' Test labels (example data)
#' @format A factor or character vector like \code{train_labels}.
#' @usage data(test_labels)
#' @docType data
#' @keywords datasets
"test_labels"
