# R/data.R

#' CRC features
#'
#' A data frame containing numeric features for colorectal cancer (CRC) samples.
#' Each row corresponds to a sample, and each column corresponds to a feature
#' extracted from the dataset.
#'
#' @format A data frame with \eqn{N} rows (samples) and \eqn{P} columns (features).
#' @details
#' This dataset is used in examples to demonstrate how to combine feature
#' data frames with class labels using the `LoadData()` function.
#'
#' @source Generated or preprocessed internally from CSV input files.
#'
#' @examples
#' data(CRCFeature)
#' head(CRCFeature)
"CRCFeature"


#' CRC labels
#'
#' A factor vector containing the class labels corresponding to each sample
#' in \code{CRCFeature}. Each element indicates the diagnostic or molecular
#' subtype of the CRC sample.
#'
#' @format A factor vector of length \eqn{N}.
#' @seealso \code{\link{CRCFeature}}, \code{\link{LoadData}}
#'
#' @examples
#' data(CRCLabel)
#' table(CRCLabel)
"CRCLabel"
