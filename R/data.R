# R/data.R

#' Training embeddings (example data)
#'
#' Example histopathology feature embeddings computed from hematoxylin and eosin
#' (H&E) regions of interest (ROIs). Each row represents a single ROI patch and
#' each column corresponds to a numerical feature extracted using a pathology
#' foundation model. The embeddings represent nine colorectal tissue classes—
#' adipose, background, debris, lymphocytes, mucus, smooth muscle, normal mucosa,
#' cancer-associated stroma, and tumor epithelium—derived from 86 FFPE colorectal
#' slides released by Kather, Halama, and Marx (2018) and processed using the
#' foundation model introduced by Chen et al. (2024).
#'
#' @format A \code{data.frame} with 90 rows (samples) and 1024 columns (features).
#' @usage data(train_embeddings)
#'
#' @references
#' Chen, R. J., Ding, T., Lu, M. Y., Williamson, D. F. K., Jaume, G.,
#' Song, A. H., Chen, B., Zhang, A., Shao, D., Shaban, M., Williams, M.,
#' Oldenburg, L., Weishaupt, L. L., Wang, J. J., Vaidya, A., Le, L. P., Gerber,
#' G., Sahai, S., Williams, W., & Mahmood, F. (2024). Towards a general-purpose
#' foundation model for computational pathology. Nature Medicine, 30(3), 850–862.
#' https://doi.org/10.1038/s41591-024-02857-3
#'
#' Kather, J. N., Halama, N., & Marx, A. (2018). 100,000 histological images of
#' human colorectal cancer and healthy tissue (v0.1) \[Data set\]. Zenodo.
#' https://doi.org/10.5281/zenodo.1214456
#'
#' @note
#' Documentation and code debugging processes were refined with the assistance
#' of ChatGPT-5 to improve accuracy and clarity.
#'
#' @docType data
#' @keywords datasets
"train_embeddings"


#' Training Labels (Example Data)
#'
#' Example class labels corresponding to \code{\link{train_embeddings}}.
#' Each label indicates the diagnostic category associated with the
#' corresponding histopathology region of interest (ROI).
#'
#' @format
#' A factor or character vector of length \eqn{n}, where each element
#' corresponds to one ROI in \code{train_embeddings}.
#'
#' @usage
#' data(train_labels)
#'
#' @seealso
#' \code{\link{train_embeddings}}
#'
#' @note
#' Documentation and code debugging processes were refined with the assistance
#' of ChatGPT-5 to improve accuracy and clarity.
#'
#' @docType data
#' @keywords datasets
"train_labels"

#' Test Embeddings (Example Data)
#'
#' @description
#' Example histopathology feature embeddings used for model evaluation. These
#' embeddings share the same structure and nine-class tissue representation as
#' \code{\link{train_embeddings}}, derived from 86 FFPE colorectal slides released
#' by Kather, Halama, and Marx (2018) and processed using the pathology foundation
#' model introduced by Chen et al. (2024).
#'
#' @format
#' A \code{data.frame} with 45 rows (samples) and 1024 columns (features).
#' @usage data(train_embeddings)
#'
#' @usage
#' data(test_embeddings)
#'
#' @references
#' Chen, R. J., Ding, T., Lu, M. Y., Williamson, D. F. K., Jaume, G.,
#' Song, A. H., Chen, B., Zhang, A., Shao, D., Shaban, M., Williams, M.,
#' Oldenburg, L., Weishaupt, L. L., Wang, J. J., Vaidya, A., Le, L. P., Gerber,
#' G., Sahai, S., Williams, W., & Mahmood, F. (2024). Towards a general-purpose
#' foundation model for computational pathology. Nature Medicine, 30(3), 850–862.
#' https://doi.org/10.1038/s41591-024-02857-3
#'
#' Kather, J. N., Halama, N., & Marx, A. (2018). 100,000 histological images of
#' human colorectal cancer and healthy tissue (v0.1) \[Data set\]. Zenodo.
#' https://doi.org/10.5281/zenodo.1214456
#'
#' @note
#' Documentation and code debugging processes were refined with the assistance
#' of ChatGPT-5 to improve accuracy and clarity.
#'
#' @docType data
#' @keywords datasets
"test_embeddings"


#' Test Labels (Example Data)
#'
#' @description
#' Example class labels corresponding to \code{\link{test_embeddings}}.
#' Each label specifies the diagnostic category for a test region of interest (ROI),
#' used to assess model performance on unseen data.
#'
#' @format
#' A factor or character vector of length \eqn{n}, matching the rows in
#' \code{\link{test_embeddings}}.
#'
#' @usage
#' data(test_labels)
#'
#' @seealso
#' \code{\link{test_embeddings}}
#'
#' @note
#' Documentation and code debugging processes were refined with the assistance
#' of ChatGPT-5 to improve accuracy and clarity.
#'
#' @docType data
#' @keywords datasets
"test_labels"
