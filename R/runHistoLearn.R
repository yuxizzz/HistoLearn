#' Launch Shiny App for HistoLearn
#'
#' Launches the Shiny application bundled with the **HistoLearn** package.
#' The app provides an interface for clustering, visualization, and training
#' downstream k-NN classification models on histology embeddings.
#'
#' @return No return value; opens a Shiny application in the user's browser.
#'
#' @examples
#' \dontrun{
#' runHistoLearn()
#' }
#'
#' @author Yuxi Zhu, \email{yuxi.zhu@mail.utoronto.ca}
#'
#' @references
#' Chen, R. J., Ding, T., Lu, M. Y., Williamson, D. F. K., Jaume, G.,
#' Song, A. H., Chen, B., Zhang, A., Shao, D., Shaban, M., Williams, M.,
#' Oldenburg, L., Weishaupt, L. L., Wang, J. J., Vaidya, A., Le, L. P., Gerber,
#' G., Sahai, S., Williams, W., & Mahmood, F. (2024). Towards a general-purpose
#' foundation model for computational pathology. Nature Medicine, 30(3), 850–862.
#' https://doi.org/10.1038/s41591-024-02857-3
#'
#' Kuhn, M. (2008). Building Predictive Models in R Using the caret
#' Package. Journal of Statistical Software, 28(5), 1–26.
#' https://doi.org/10.18637/jss.v028.i05
#'
#' R Core Team (2025). _R: A Language and Environment for
#' Statistical Computing_. R Foundation for Statistical Computing,
#' Vienna, Austria. <https://www.R-project.org/>.
#'
#' H. Wickham. ggplot2: Elegant Graphics for Data Analysis.
#' Springer-Verlag New York, 2016.
#'
#' Sarkar D (2008). _Lattice: Multivariate Data Visualization with
#' R_. Springer, New York. ISBN 978-0-387-75968-5,
#' <http://lmdvr.r-forge.r-project.org>.
#'
#' ChatGPT (2025). ChatGPT 5.1 (February 2025 Release).
#' OpenAI, San Francisco, CA. <https://openai.com/>.
#'
#' @export
#' @importFrom shiny runApp
runHistoLearn <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "HistoLearn")
  shiny::runApp(appDir, display.mode = "normal")
  return(invisible(NULL))
}
