#' Visualize Embeddings Using Reduced Dimensions
#'
#' Generate a 2D scatterplot or a multi-dimensional paired plot of histological
#' feature embeddings using dimensionality reduction. Currently supports
#' Principal Component Analysis (PCA) for projection.
#'
#' @param input_data An object of class \code{"histofeature"} created by
#'   \code{\link{load_embeddings}}.
#' @param dimensions A positive integer specifying the number of dimensions
#'   to visualize (between 2 and 8). Defaults to \code{2}.
#' @param type The dimensionality reduction method to use. Currently only
#'   \code{"pca"} is supported.
#'
#' @return A \code{ggplot} object (for 2D visualization) or a
#'   \code{ggmatrix} object (for multi-dimensional paired plots), displaying
#'   the projected embeddings colored by label.
#'
#' @examples
#' data(train_embeddings)
#' data(train_labels)
#' train_set <- load_embeddings(feature=train_embeddings, label=train_labels)
#' visualize_embeddings(train_set)
#'
#' @references
#' R Core Team (2025). _R: A Language and Environment for
#' Statistical Computing_. R Foundation for Statistical Computing,
#' Vienna, Austria. <https://www.R-project.org/>.
#'
#' H. Wickham. ggplot2: Elegant Graphics for Data Analysis.
#' Springer-Verlag New York, 2016.
#'
#' Schloerke B, Cook D, Larmarange J, Briatte F, Marbach M, Thoen E,
#' Elberg A, Crowley J (2025). _GGally: Extension to 'ggplot2'_.
#' doi:10.32614/CRAN.package.GGally
#' <https://doi.org/10.32614/CRAN.package.GGally>, R package
#' version 2.4.0, <https://CRAN.R-project.org/package=GGally>.
#'
#' @note
#' Documentation and debugging processes were refined with the assistance
#' of ChatGPT-5 to improve accuracy and clarity.
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
