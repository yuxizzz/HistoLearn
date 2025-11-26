#' Visualize Embeddings Using Reduced Dimensions
#'
#' Project histological feature embeddings to a low-dimensional space and
#' visualize them as either a 2D scatterplot (when \code{dimensions = 2}) or
#' a matrix of pairwise scatterplots (when \code{dimensions > 2}). The function
#' currently uses Principal Component Analysis (PCA) for dimensionality
#' reduction and colors points by their associated labels.
#'
#' @param input_data An object of class \code{"histofeature"} as returned by
#'   \code{\link{load_embeddings}}. It must contain a numeric feature matrix
#'   in \code{input_data$feature} and a non-\code{NULL} label vector/factor in
#'   \code{input_data$label}.
#' @param dimensions Integer scalar specifying the number of dimensions to
#'   visualize. Must be between \code{2} and \code{10}. If the requested number
#'   of dimensions exceeds the number of available principal components, the
#'   maximum available is used instead.
#' @param type Character string indicating the dimensionality reduction method.
#'   Currently only \code{"pca"} is supported.
#'
#' @examples
#' \dontrun{
#' data(train_embeddings)
#' data(train_labels)
#' train_set <- load_embeddings(feature=train_embeddings, label=train_labels)
#' visualize_embeddings(train_set)
#' }
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
#' OpenAI. (2025). ChatGPT (GPT-5.1, February 2025 version)
#' (Large language model). <https://chat.openai.com/>
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
  # Validate input_data object
  if (!inherits(input_data, "histofeature")) {
    stop("invalid input", call. = FALSE)
  }
  # Convert dimensions to integer for safety
  k <- as.integer(dimensions)
  # input checking
  if (k > 10 || k < 2) {
    stop("Not informative or invalid. Choose dimension within 2 to 10", call.=TRUE)
  }

  # Dimensionality reduction (currently supports only PCA)
  if (type=='pca') {
    pca_result <- stats::prcomp(input_data$feature, center = TRUE, scale. = TRUE)
    k <- min(k, ncol(pca_result$x))
    reduced_dim <- as.data.frame(pca_result$x[, seq_len(k), drop = FALSE])
    colnames(reduced_dim) <- paste0("dim", seq_len(k))
    reduced_dim$label <- input_data$label
  } else {
    stop("not supported", call.= FALSE)
  }
  # Visualization: ore than 2 dimensions -> produce GGally pairwise scatterplot matrix
  if (k > 2) {
    p <- GGally::ggpairs(
      reduced_dim,
      columns = seq_len(k),
      aes(color = reduced_dim$label)) +
      ggplot2::theme_minimal() +
      ggplot2::ggtitle(paste("Paired", type, " projection with dimension of", k))
  } else {
    # Exactly 2 dimensions -> produce 2D scatterplot
    p <- ggplot2::ggplot(reduced_dim, aes(reduced_dim$dim1, reduced_dim$dim2,
                                          color = reduced_dim$label)) +
      ggplot2::geom_point(size = 2, alpha = 0.8) +
      ggplot2::theme_minimal() +
      ggplot2::ggtitle(paste(type, " projection with dimension of", k))
  }
  return(p)
}
#[END]
