<!-- badges: start -->
  [![Travis build status](https://travis-ci.org/uyedaj/treedata.table.svg?branch=master)](https://travis-ci.org/uyedaj/treedata.table)
  <!-- badges: end -->

# treedata.table: An R package for manipulating phylogenetic data with _data.table_
A wrapper for data.table that enables fast manipulation of  phylogenetic trees matched to data.

The [`data.table` package](https://github.com/Rdatatable/data.table) enables high-performance extended functionality for 
data tables in R. `treedata.table` is a wrapper for `data.table` for phylogenetic analyses that matches a phylogeny to the 
data.table, and preserves matching during `data.table` operations.

## Basic Installation

If you just want to use the treedata.table package within R follow these instuctions run the following commands in R. 

```r
library(devtools)
install_github("uyedaj/treedata.table")
```

## Contribution guidelines 

git2rdata welcomes contributions. Please read our Contributing guidelines first. The git2rdata project has a Contributor Code of Conduct. By contributing to this project, you agree to abide by its terms.


### Fixing typos

Small typos or grammatical errors in documentation may be edited directly using the GitHub web interface.

### Prerequisites

Please file an issue and make any of the developers agrees that it’s a problem. If you’ve found a bug, feel free to create an associated issue!


### Pull request process

Please (1) create a Git branch for each pull request. (2) Check the Travis status before and after making changes (integrated in the README). (3) Please don't restyle code that has nothing to do with your PR. (4) Use roxygen2 and testthat. (5) If possible, include a test case. 
