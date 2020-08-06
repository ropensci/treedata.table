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
      extractVector(td, 'SVL'),
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

test_that("Column containing tip labs can be correctly detected", {
  tre <- anolis$phy
  dat1 <- anolis$dat
  dat2 <- dat1[, sample(ncol(dat1), ncol(dat1))]
  td1 <- as.treedata.table(tre, dat1)
  td2 <- as.treedata.table(tre, dat2)
  expect_equal(td1$phy, td2$phy)
})

test_that("Find the correct number of discrete/continuous characters in the anolis dataset", {
  expect_equal(detectCharacterType(anolis$dat[,1]), detectAllCharacters(anolis$dat)[1])
})



test_that("head() returns a data.table object",{
  expect_is(head(td), "data.table")
})

test_that("tail() returns a data.table object",{
  expect_is(tail(td), "data.table")
})

test_that("Error is shown when tips with different tip labels are used",{
  anolis2 <- anolis$phy
  anolis2$tip.label[1] <- "NAA"
  tree2<-list(anolis$phy,anolis2)
  class(tree2)<-'multiPhylo'
  expect_error(as.treedata.table(tree = tree2, data = as.anolis$dat),
  "Please make sure that tip labels are equivalent across trees in the multiPhylo object \n", fixed=T)
})

test_that("Error is a non-phylo (or multiPhylo) object is used in the phy",{
  expect_error(as.treedata.table(tree = anolis$dat, data = as.anolis$dat),
               "Please use a class 'phylo' or 'multiPhylo' tree \n", fixed=T)
})



test_that("Error is a non-data.frame is used in as.treedata.table",{
  expect_error(as.treedata.table(tree = anolis$phy, data = as.matrix(anolis$dat)),
               "Your data MUST be of class data.frame", fixed=T)
})


test_that("Normal as.treedata.table",{
  expect_is(as.treedata.table(tree = anolis$phy, data = anolis$dat),
            "treedata.table")

})


test_that("Normal as.treedata.table but data without column names",{
  data=anolis$dat
  colnames(data)<-NULL
  expect_is(as.treedata.table(tree = anolis$phy, data = data),
            "treedata.table")

})


test_that("Normal as.treedata.table but testing the no tips dropped message",{
  expect_message(as.treedata.table(tree = anolis$phy, data = anolis$dat),
            "No tips were dropped from the original tree/dataset", fixed=T)

})


test_that("Normal as.treedata.table but testing if the tips dropped message is shown for trees dropped from tree",{
  anolis1 <- anolis$phy
  anolis1$tip.label[1] <- "NAA"

  expect_message(as.treedata.table(tree = anolis1, data = anolis$dat),
                 " tip(s) dropped from the original tree", fixed=T)

})


test_that("Normal as.treedata.table but testing if the tips dropped message is shown for trees dropped from data",{
  anolis1 <- anolis$phy
  anolis1$tip.label[1] <- "NAA"

  expect_message(as.treedata.table(tree = anolis1, data = anolis$dat),
                 " dropped from the original dataset", fixed=T)

})




