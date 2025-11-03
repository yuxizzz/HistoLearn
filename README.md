# HistoLearn

## Description:

A paragraph that describes the purpose of your R package and biological
data being analyzed. Explain how your package add to or improve a
current work flow in bioinformatics or computa- tional biology (i.e.,
how is it unique?, what issue does it address?). Finally, include the R
version (not RStudio version) and platform (Mac, Windows, Linux (Debian,
Fedora/Redhat, Ubuntu)), used to develop the package. You may obtain
this information by running utils::sessionInfo(). There should be no
Shiny implementation at this point.

HistoLearn provides a lightweight, modular workflow for computational
pathology that integrates core steps of feature handling and model
evaluation: loading H&E region-of-interest (ROI) embeddings, performing
dimensionality reduction, training a classifier, and assessing
performance. The package implements a simple yet reproducible PCA + k-NN
pipeline inspired by common workflows for pathology foundation models
(Chen et al., 2024). Key strengths include: - A minimalist pipeline for
interpreting high-dimensional histological features derived from
pathology foundation models. - One-line downstream training of a
supervised machine-learning model. - Intuitive visualization of
embedding spaces through PCA-based projections and pairwise plots.

This package was developed under R 4.5.1 (2025-06-13) on macOS Sonoma
14.5 (aarch64-apple-darwin20).

## Installation

To install the latest version of the package:

```         
install.packages("devtools")
library("devtools")
devtools::install_github("yuxizzz/HistoLearn", build_vignettes = TRUE)
library("HistoLearn")
```

To run the shinyApp: Under construction

## Overview:

Provide the following commands, customized to your R package. Then
provide a list of user accessible functions within the package in order
of use and a brief description of each. Include one image illustrating
the overview of the package that shows the inputs and outputs. Ensure
the image is deposited in the correct location, as discussed in class.
Point the user to vignettes for a tutorial of your package. For an
example, see TestingPackage.

Full tutorial on vignettes.

```         
ls("package:HistoLearn")
data(package = "HistoLearn") # optional
browseVignettes("HistoLearn")
```

### Overview diagram

### Main functions

| Function | Description |
|---------------------------------|---------------------------------------|
| `load_embeddings(feature, label = NULL)` | Load features and labels into a `"histofeature"` object with validation. |
| `visualize_embeddings(input_data, dimensions = 2, type = "pca")` | Perform PCA projection and generate 2D or pairwise embedding plots. |
| `train_model(feature_embedding, dr = "pca", dr_k = 20, model = "knn")` | Apply PCA reduction and train a k-NN classifier. |
| `evaluate_model(trained_model, test_data)` | Evaluate predictions on a test set, visualize confusion matrix, and compute accuracy. |
| `reduce_dim(X, method = "pca", k = 20)` | Internal helper for PCA-based dimensionality reduction. |
| `train_knn(X, y)` | Internal helper for k-NN tuning and model fitting. |

### Example Data

-   `train_embeddings`, `train_labels` — training set (9 human
    colorectal cancer issue classes) from Kather, Halama, and Marx
    (2018).
-   `test_embeddings`, `test_labels` — independent test set for model
    evaluation.

## Contributions:

-   Provide a paragraph clearly indicating the name of the author of the
    package and contri- butions from the author.
-   Outline contributions from other packages/sources for each function.
-   Outline contributions from generative AI tool(s) for each function.
    Include how the tools were used and how the results from AI tools
    were incorporated.
-   Remember your individual contributions to the package are important.

Author: Yuxi Zhu
- Designed and implemented the full codebase and pipeline for the package.
- Conducted data preprocessing to prepare example embeddings and structured training/test datasets.
- Authored all documentation, vignettes, and example workflows.

External package contributions
- stats: PCA computation. 
- caret: k-NN model training and cross-validation. 
- ggplot2, GGally: visualization functions. 

Example data 
- derived from Kather et al. (2018) and processed using the pathology foundation model from Chen et
al. (2024).

Use of generative AI (ChatGPT-5) 
- Used to refine documentation formatting, debugging code and looking up functions 

## References:

```         
Chen, R. J., Ding, T., Lu, M. Y., Williamson, D. F. K., Jaume, G.,
Song, A. H., Chen, B., Zhang, A., Shao, D., Shaban, M., Williams, M.,
Oldenburg, L., Weishaupt, L. L., Wang, J. J., Vaidya, A., Le, L. P., Gerber,
G., Sahai, S., Williams, W., & Mahmood, F. (2024). Towards a general-purpose
foundation model for computational pathology. Nature Medicine, 30(3), 850–862.
https://doi.org/10.1038/s41591-024-02857-3

Kuhn, M. (2008). Building Predictive Models in R Using the caret Package. 
Journal of Statistical Software, 28(5), 1–26. https://doi.org/10.18637/jss.v028.i05

R Core Team (2025). _R: A Language and Environment for
Statistical Computing_. R Foundation for Statistical Computing,
Vienna, Austria. <https://www.R-project.org/>.

H. Wickham. ggplot2: Elegant Graphics for Data Analysis.
Springer-Verlag New York, 2016.

Sarkar D (2008). _Lattice: Multivariate Data Visualization with
R_. Springer, New York. ISBN 978-0-387-75968-5,
<http://lmdvr.r-forge.r-project.org>.

Schloerke B, Cook D, Larmarange J, Briatte F, Marbach M, Thoen E,
Elberg A, Crowley J (2025). _GGally: Extension to 'ggplot2'_.
doi:10.32614/CRAN.package.GGally
<https://doi.org/10.32614/CRAN.package.GGally>, R package
version 2.4.0, <https://CRAN.R-project.org/package=GGally>.

Kather, J. N., Halama, N., & Marx, A. (2018). 100,000 histological images of
human colorectal cancer and healthy tissue (v0.1) \[Data set\]. Zenodo.
https://doi.org/10.5281/zenodo.1214456
```

## Acknowledgements:

This package was developed as part of an assessment for 2025 BCB410H:
Applied Bioinformat- ics course at the University of Toronto, Toronto,
CANADA. HistoLearn welcomes issues, enhancement requests, and other
contributions. To submit an issue, use the GitHub issues.

## Other Topics:
