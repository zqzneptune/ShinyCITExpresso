# ShinyCITExpresso
A ShinyApp for visualization and explorering Cellular Indexing of Transcriptomes and Epitopes by Sequencing (CITE-seq) data

## Installation
```r
# install devtools
install.packages("devtools")

# install graphics and data manipulation packages
install.packages(c("dplyr", "ggrepel", "magrittr", "plyr", "ggplot2", "RColorBrewer"))

# install ShinyApp related packages
install.packages(c("shiny", "waiter", "bslib", "bsicons"))

# install Bioconductor packages
install.packages("BiocManager")
BiocManager::install(
  c("SummarizedExperiment", "BiocGenerics",
    "SingleCellExperiment", "MultiAssayExperiment",
    "HDF5Array")
)

```
