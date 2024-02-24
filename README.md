
# ShinyCITExpresso

![](img/ShinyCITExpresso_title_QZ_main.png)

The `ShinyCITExpresso` R package provides a web application based on
[Shiny](https://shiny.posit.co/) and
[bslib](https://github.com/rstudio/bslib/) for visualization CITE-seq
multiomics data:

- Supports
  [MultiAssayExperiment](https://waldronlab.io/MultiAssayExperiment/index.html)
  objects

- Runs on [Rstudio Desktop](https://posit.co/download/rstudio-desktop/),
  or online platforms supporting [Shiny
  Server](https://posit.co/products/open-source/shinyserver/)

- Designed to integrate results from diverse CITE-seq analysis pipelines, including:

  - Bioconductor
  - Seurat
  - MuData

## Navigating Cellular Data: A Visual Journey

![](img/Action_Overview.gif)

## Tailored Insights: Visualizing Custom Features

![](img/Action_Featureviz.gif)

## Harmonizing Signals: Understanding Cross-Modality Correlation

![](img/Action_Crossmoda.gif)

## Virtual Flow Cytometry: Decoding Pseudo FACS

![](img/Action_PseudoSort_1.gif)


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
