<!-- badges: start -->
  [![Travis build status](https://travis-ci.org/uyedaj/treedata.table.svg?branch=master)](https://travis-ci.org/uyedaj/treedata.table)
"[![COVER](https://coveralls.io/repos/github/uyedaj/treedata.table/badge.svg?branch=master)](https://coveralls.io/github/uyedaj/treedata.table)"
[![Devel version](https://img.shields.io/badge/devel%20version-0.1.0-blue.svg)](https://github.com/uyedaj/treedata.table)
[![Lifecycle](https://img.shields.io/badge/lifecycle-uyedaj/treedata.table-blue.svg)](https://www.tidyverse.org/lifecycle/#uyedaj/treedata.table)
[![Code size](https://img.shields.io/github/languages/code-size/uyedaj/treedata.table.svg)](https://github.com/uyedaj/treedata.table)
[![Latest commit](https://img.shields.io/github/last-commit/uyedaj/treedata.table.svg)](https://github.com/uyedaj/treedata.table/commits/master)
[![Dependencies](https://tinyverse.netlify.com/badge/uyedaj/treedata.table)](https://cran.r-project.org/package=uyedaj/treedata.table)
[![R build status](https://github.com/uyedaj/treedata.table/workflows/R-CMD-check/badge.svg)](https://github.com/uyedaj/treedata.table/actions)
<!-- badges: end -->

# treedata.table: An R package for manipulating phylogenetic data with _data.table_
A wrapper for data.table that enables fast manipulation of  phylogenetic trees matched to data.

The [`data.table` package](https://github.com/Rdatatable/data.table) enables high-performance extended functionality for 
data tables in R. `treedata.table` is a wrapper for `data.table` for phylogenetic analyses that matches a phylogeny to the 
data.table, and preserves matching during `data.table` operations.

# Installing `treedata.table`

treedata.table can be installed from GitHub at the present. We presently recommend installing using
[`remotes`](https://cran.r-project.org/web/packages/remotes/index.html):

```{r}
 remotes::install_github("uyedaj/treedata.table")
 ```

# What Can I Do With `treedata.table`?

`treedata.table` is designed with the intention of being able to efficiently manipulate trait data and
phylogenetic trees to enable comparative analyses. With the package is bundled some example data. Let's load it in and look at some common analyses.

```{r}
data(anolis)
td <- as.treedata.table(tree = anolis$phy, data = anolis$dat)
```

The function `as.treedata.table` converts a normal comma- or tab-delimited file to the `data.table` [format](https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html). This enables a range of efficient and intutive indexing and selection operations. 

As an example, in our dataset is the column 'SVL', or snout-to-vent length in our anoles. We can index out this column on the fly, and run a Brownian motion analysis on this trait using the R package Geiger:

```{r}
tdt(td, geiger::fitContinuous(phy, extractVector(td, 'SVL'), model="BM", ncores=1))
```

We can also do efficient dropping of taxa from the analysis like so:

```{r}
dt <- droptreedata.table(tdObject=td, taxa=c("chamaeleonides" ,"eugenegrahami" ))
```

## tl;dr

`treedata.table` is a library for syncing data between a dataset and the tipcs of trees to enable efficient data and taxon management, as well as on-the-fly indexing and data selection in comparative analyses.

# Contributing. 

Please see our contributing guide.

# Contact

Please see the package DESCRIPTION for package authors.

