# treedata.table
##  An R package for manipulating phylogenetic data with _data.table_
A wrapper for data.table that enables fast manipulation of  phylogenetic trees matched to data. <img src='man/figures/logo.png' align="right" height="250" />

<!-- badges: start -->
  [![Travis build status](https://travis-ci.org/uyedaj/treedata.table.svg?branch=master)](https://travis-ci.org/uyedaj/treedata.table)
[![codecov](https://codecov.io/gh/uyedaj/treedata.table/branch/master/graph/badge.svg)](https://codecov.io/gh/uyedaj/treedata.table)
[![Devel version](https://img.shields.io/badge/devel%20version-0.1.0-blue.svg)](https://github.com/uyedaj/treedata.table)
[![Lifecycle](https://img.shields.io/badge/lifecycle-uyedaj/treedata.table-blue.svg)](https://www.tidyverse.org/lifecycle/#uyedaj/treedata.table)
[![Code size](https://img.shields.io/github/languages/code-size/uyedaj/treedata.table.svg)](https://github.com/uyedaj/treedata.table)
[![Latest commit](https://img.shields.io/github/last-commit/uyedaj/treedata.table.svg)](https://github.com/uyedaj/treedata.table/commits/master)
<!-- badges: end -->

The [`data.table` package](https://github.com/Rdatatable/data.table) enables high-performance extended functionality for 
data tables in R. `treedata.table` is a wrapper for `data.table` for phylogenetic analyses that matches a phylogeny to the 
data.table, and preserves matching during `data.table` operations.

## Why to use `treedata.table`?

Although the simulatenous processing of phylogenetic trees compromises `treedata.table`'s speed by **90%** (relative to `data.table`; *Fig. 1A*), data manipulation in `treedata.table` is significantly faster than in other commonly used packages such as `base` (**>35%**), `treeplyr` (**>60%**), and `dplyr` (**>90%**). Additionally, `treedata.table` is **>400%** faster than `treeplyr` during the initial data/tree matching step (*Fig. 1B*).  

<div style="text-align:center">
<img src='man/figures/bench_TDT_Aug14.png' align="middle"width="600" />
</div>

 <font size="2"> **Fig. 1.** Results for the `treedata.table` microbenchmark during (**A**) data manipulation (`treedata.table[`) and (**B**) tree/data matching steps. We compare the performance of `treedata.table` against `treedata.table`, `base`, `treeplyr`, and `dplyr` using the `microbenchmark` R package.</font> 


## Installing `treedata.table`

treedata.table can be installed from GitHub at the present. We presently recommend installing using
[`remotes`](https://cran.r-project.org/web/packages/remotes/index.html):

```{r}
 remotes::install_github("uyedaj/treedata.table")
 ```

## What Can I Do With `treedata.table`?

`treedata.table` is designed with the intention of being able to efficiently manipulate trait data and
phylogenetic trees to enable comparative analyses. With the package is bundled some example data. Let's load it in and look at some common analyses.

```{r}
data(anolis)
td <- as.treedata.table(tree = anolis$phy, data = anolis$dat)
```

The function `as.treedata.table` converts a normal comma- or tab-delimited file to the `data.table` [format](https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html). This enables a range of efficient and intutive indexing and selection operations. 

As an example, in our dataset is the column `SVL`, or snout-to-vent length in our anoles. We can index out this column on the fly, and run a Brownian motion analysis on this trait using the R package Geiger:

```{r}
tdt(td, geiger::fitContinuous(phy, extractVector(td, 'SVL'), model="BM", ncores=1))
```

We can also do efficient dropping of taxa from the analysis like so:

```{r}
dt <- droptreedata.table(tdObject=td, taxa=c("chamaeleonides" ,"eugenegrahami" ))
```

### tl;dr

`treedata.table` is a library for syncing data between a dataset and the tipcs of trees to enable efficient data and taxon management, as well as on-the-fly indexing and data selection in comparative analyses.

## Contributing

Please see our [contributing guide](CONTRIBUTING).

## Contact

Please see the package [DESCRIPTION](DESCRIPTION) for package authors.

