# HistoLearn

## Description:
A paragraph that describes the purpose of your R package and biological data being analyzed.
Explain how your package add to or improve a current work flow in bioinformatics or computa-
tional biology (i.e., how is it unique?, what issue does it address?). Finally, include the R version
(not RStudio version) and platform (Mac, Windows, Linux (Debian, Fedora/Redhat, Ubuntu)),
used to develop the package. You may obtain this information by running utils::sessionInfo().
There should be no Shiny implementation at this point.

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

Provide the following commands, customized to your R package. Then provide a list of user
accessible functions within the package in order of use and a brief description of each. Include
one image illustrating the overview of the package that shows the inputs and outputs. Ensure
the image is deposited in the correct location, as discussed in class. Point the user to vignettes
for a tutorial of your package. For an example, see TestingPackage.
```
ls("package:<PackageName>")
data(package = "<PackageName>") # optional
browseVignettes("<PackageName>")
```

## Contributions:
* Provide a paragraph clearly indicating the name of the author of the package and contri-
butions from the author.
* Outline contributions from other packages/sources for each function.
* Outline contributions from generative AI tool(s) for each function. Include how the tools
were used and how the results from AI tools were incorporated.
* Remember your individual contributions to the package are important.

## References:
Provide full references for all sources used, including for the packages or tools mentioned
under ’Contributions’, in one specific format.

## Acknowledgements:
Provide the following text, customized to your R package.
This package was developed as part of an assessment for 2025 BCB410H: Applied Bioinformat-
ics course at the University of Toronto, Toronto, CANADA. <PackageName> welcomes issues,
enhancement requests, and other contributions. To submit an issue, use the GitHub issues.

## Other Topics:
Any other content of your preference must come after ‘Acknowledgements’ section, with mean-
ingful headings of your choice.

