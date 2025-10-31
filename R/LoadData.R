#' Calculates Information Criteria
#'
#' A function that calculates information criteria, BIC (Bayesian
#' Information Criterion) and AIC (Akaike Information Criterion)
#' given log-likelihood, number of clusters, dimensionality of the
#' dataset and the number of observations.
#'
#' @param datapath The file path for the input data, the data should be
#' in csv, tsv format
#'
#' @return Returns an S3 object of class InfCriteria with results.
#' \itemize{
#'   \item BICresults - A value of class numeric indicating BIC value.
#'   \item AIC results - A value of class numeric indicating AIC value.
#' }
#'
#' @examples
#' HistData <- LoadData(filepath)
#' # Access BIC value
#' HistData$feature
#' HistData$label
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
LoadData <- function(filepath, label=NULL) {

  # calculate number of parameters based on Gaussian mixture
  data <- utils::read.csv(filepath)
  if (label != NULL) {
    label <- read.csv(label)
  }

  ## Check if they are the same dimension

  # create object
  Results <- list(feature = data,
                  label = label)

  class(Results) <- "HistFeature"
  return(Results)
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
