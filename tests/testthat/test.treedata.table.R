context("String length")
library(treedata.table)
library(ape)

data(anolis)
td <- as.treedata.table(tree = anolis$phy, data = anolis$dat)
tdt_output <-
  tdt(
    td,
    geiger::fitContinuous(
      phy,
      extractVector(td, SVL),
      model = "BM",
      ncores = 1
    )
  )

test_that("The resulting td object is of class treedata.table", {
  expect_is(td, "treedata.table")
})

test_that("Trees are the same", {
  expect_true(all.equal.phylo(td$phy, anolis$phy))
})

test_that("Species in tree and trait dataframe are the same", {
  expect_equal(td$phy$tip.label, td$dat$tip.label)
})


test_that("datasets are the same", {
  expect_true(all(colnames(anolis$dat)[-1] == colnames(as.data.frame(td$dat))[-1]))
})


test_that("Check the match between tree and dataset", {
  expect_true(attr(td, "tree_not_data") == "OK")
  expect_true(attr(td, "data_not_tree") == "OK")
})


test_that("tdt function works fine", {
  expect_is(tdt_output, "list")
})

test_that("Extracting a single column from the treedata.table object", {
  expect_equal(ncol(td[, SVL]$dat), 1)
})

test_that(
  "The  number of rows is the same after filtering the original and tdt object
          under the same criteria",
  {
    expect_equal(
      nrow(anolis$dat[anolis$dat$island == "Cuba" &
        anolis$dat$ecomorph == "TG", ]),
      nrow(td[island == "Cuba" & ecomorph == "TG", ]$dat)
    )
  }
)

test_that("[[ extracts a named character vector", {
  expect_is(names(td[["SVL"]]), "character")
  expect_is(td[["SVL"]], "numeric")
})



test_that("phy and dat objects can be extracted correctly using pull.treedata.table", {
  expect_is(pull.treedata.table(td, type = "phy"), "phylo")
  expect_is(pull.treedata.table(td, type = "dat"), "data.table")
})


