#' Load in Histological Features
#'
#' A function that load in histological features extracted from any
#' pathology foundation model. Optionally include labels
#' given log-likelihood, number of clusters, dimensionality of the
#' dataset and the number of observations.
#'
#' @param feature The file path for the input data, the data should be
#' in csv, tsv format
#' @param label The file path for the input data, the data should be
#' in csv, tsv format
#' @param label_col The file path for the input data, the data should be
#' in csv, tsv format
#' @return Returns a dataframe with feature columns and one label column
#'
#' @examples
#' data(CRCFeature)
#' data(CRCLabel)
#' HistData <- LoadData(CRCFeature, CRCLabel)
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
LoadData <- function(feature, label = NULL, label_col = "label") {
  # calculate number of parameters based on Gaussian mixture
  # create object
  result <- feature

  if (!is.null(label)){
    if (is.data.frame(label) && ncol(label) == 1L) label <- label[[1]]
    if (is.matrix(label) && ncol(label) == 1L) label <- label[, 1]
    labels_fac  <- as.factor(label)
    result[[label_col]] <- labels_fac
  }
  rownames(result) <- NULL
  return(result)
}

# need to wrap around dontrun if the example code
# use packages outside of the base R/

# @export & @importFrom specify the R packages the
# function relied on

# outputInfCriteria <- InfCriteriaV3(loglikelihood = -5080,
#                                     clusters = 2,
#                                     dimensions = 3,
#                                     nObservations = 1000)
# outputInfCriteria$BICresults
# outputInfCriteria$AICresults
#[END]
