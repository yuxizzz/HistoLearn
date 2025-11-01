#' Visualize the command
#'
#' A function that calculates information criteria, BIC (Bayesian
#' Information Criterion) and AIC (Akaike Information Criterion)
#' given log-likelihood, number of clusters, dimensionality of the
#' dataset and the number of observations.
#'
#' @param input_data A negative value of class numeric indicating
#'   the log-likelihood.
#' @param dimensions A positive integer indicating the number of clusters.
#' @param type A positive integer indicating the dimensionality
#'   of the dataset.
#'
#'
#' @examples
#' data(train_embeddings)
#' data(train_labels)
#' train_set <- load_embeddings(feature=train_embeddings, label=train_labels)
#' visualize_embeddings(train_set)
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
#' @import stats
#' @import ggplot2
visualize_embeddings <- function(input_data,
                                  dimensions=2,
                                  type='pca') {

  if (type=='pca') {
    pca_result <- stats::prcomp(input_data$feature, center = TRUE, scale. = TRUE)
    df_pca <- data.frame(
      pc1 = pca_result$x[, 1],
      pc2 = pca_result$x[, 2],
      Label = input_data$label
    )
    ggplot2::ggplot(df_pca, aes(pc1, pc2, color = Label)) +
      geom_point(size = 2, alpha = 0.7) +
      theme_minimal() +
      ggtitle("PCA projection of the Embeddings")

  } else {
    stop("not supported")
  }
}
#[END]
