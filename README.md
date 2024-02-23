
# ShinyCITExpresso

The `ShinyCITExpresso` R package provides a web application based on
[Shiny](https://shiny.posit.co/) and
[bslib](https://github.com/rstudio/bslib/) for visualization CITE-seq
multiomics data:

- Accepts
  [MultiAssayExperiment](https://waldronlab.io/MultiAssayExperiment/index.html)
  objects.

- Runs on [Rstudio Desktop](https://posit.co/download/rstudio-desktop/),
  or online platforms supports [Shiny
  Server](https://posit.co/products/open-source/shinyserver/)

- Works with results from various CITE-seq analysis framework including:

  - Bioconductor
  - Seurat
  - MuData

- An Intuitive Design with Novel Functions

## Installation

Install Dependencies:
``` r
install.packages(c("devtools", "BiocManager"))
``` 

```r
install.packages(c("dplyr", "ggrepel", "magrittr", 
"plyr", "ggplot2", "RColorBrewer","shiny", "waiter", "bslib", "bsicons"))
```

```r
BiocManager::install(
  c("SummarizedExperiment", "BiocGenerics",
    "SingleCellExperiment", "MultiAssayExperiment",
    "HDF5Array")
)
```


Install the `ShinyCITExpresso` package from Github as follows:

``` r
devtools::install_github("zqzneptune/ShinyCITExpresso")
```

## Quick demo

Access live demo [here](https://www.citexpresso.net/shinydemo/), and explore results from Seurat WNN (Weighted Nearest Neighbor) analysis of 30,672 scRNA-seq profiles measured alongside a panel of 25 antibodies from bone marrow ([Stuart*, Butler* et al, Cell 2019](https://www.cell.com/cell/fulltext/S0092-8674(19)30559-8)). 


## Get started

Select object type to get started with your own data:

- [SingleCellExpeiment](https://www.citexpresso.net/shinydemo/)
- [Seurat](https://www.citexpresso.net/shinydemo/)
- [MuData](https://www.citexpresso.net/shinydemo/)

## Code of Conduct

Please note that the `ShinyCITExpresso` project is released with a
[Contributor Code of
Conduct](https://github.com/zqzneptune/ShinyCITExpresso/blob/main/code_of_conduct.md).
By contributing to this project, you agree to abide by its terms.
