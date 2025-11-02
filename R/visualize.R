#' Visualize Embeddings using Reduced Dimensions
#'
#' A function that creates a 2D scatterplot or a multi-dimensional paired plot
#' of embeddings.
#'
#' @param input_data an object of class \code{"histofeature"}
#' @param dimensions A A positive integer indicating the dimension to visualize,
#' ranging from 2 to 8. The default is \code{2}.
#' @param type The type of dimension reduction techniques to perform, currently
#' only support \code{"pca"}.
#' @return
#' A \code{ggplot} or \code{ggmatrix} object that visualized the relationship.
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
#' @import stats ggplot2 GGally
visualize_embeddings <- function(input_data,
                                  dimensions=2,
                                  type='pca') {
  if (!inherits(input_data, "histofeature")) {
    stop("invalid input", call. = FALSE)
  }

  k <- as.integer(dimensions)
  if (k > 10 || k < 2) {
    stop("Not informative or invalid. Choose dimension within 2 to 8", call.=TRUE)
  }
  if (is.null(input_data$label)){
    stop("Require cluster label", call.=TRUE)
  }

  if (type=='pca') {
    pca_result <- stats::prcomp(input_data$feature, center = TRUE, scale. = TRUE)
    k <- min(k, ncol(pca_result$x))

    reduced_dim <- as.data.frame(pca_result$x[, seq_len(k), drop = FALSE])
    colnames(reduced_dim) <- paste0("dim", seq_len(k))
    reduced_dim$Label <- input_data$label
  } else {
    stop("not supported", call.= FALSE)
  }

  if (k > 2) {
    p <- GGally::ggpairs(
      reduced_dim,
      columns = seq_len(k),
      aes(color = Label)) +
      ggplot2::theme_minimal() +
      ggplot2::ggtitle(paste("Paired", type, " projection with dimension of", k))
  } else {
    p <- ggplot2::ggplot(reduced_dim, aes(dim1, dim2, color = Label)) +
      ggplot2::geom_point(size = 2, alpha = 0.8) +
      ggplot2::theme_minimal() +
      ggplot2::ggtitle(paste(type, " projection with dimension of", k))
  }
  return(p)
}
#[END]
