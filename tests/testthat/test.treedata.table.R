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
      extractVector(td, "SVL"),
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
  expect_true(all(colnames(anolis$dat)[-1] ==
    colnames(as.data.frame(td$dat))[-1]))
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



test_that("phy and dat objects can be extracted correctly using
          pulltreedata.table", {
  expect_is(pulltreedata.table(td, type = "phy"), "phylo")
  expect_is(pulltreedata.table(td, type = "dat"), "data.table")
})

test_that("Column containing tip labs can be correctly detected", {
  tre <- anolis$phy
  dat1 <- anolis$dat
  dat2 <- dat1[, sample(ncol(dat1), ncol(dat1))]
  td1 <- as.treedata.table(tre, dat1)
  td2 <- as.treedata.table(tre, dat2)
  expect_equal(td1$phy, td2$phy)
})

test_that("Find the correct number of discrete/continuous characters in the
          anolis dataset", {
  expect_equal(
    detectCharacterType(anolis$dat[, 1]),
    detectAllCharacters(anolis$dat)[1]
  )
})



test_that("head() returns a data.table object", {
  expect_is(head(td), "data.table")
})

test_that("tail() returns a data.table object", {
  expect_is(tail(td), "data.table")
})

test_that("Error is shown when tips with different tip labels are used", {
  anolis2 <- anolis$phy
  anolis2$tip.label[1] <- "NAA"
  tree2 <- list(anolis$phy, anolis2)
  class(tree2) <- "multiPhylo"
  expect_error(as.treedata.table(tree = tree2, data = as.anolis$dat),
  "Tip labels must be equivalent across trees in multiPhylo object", fixed = T)
})

test_that("Error is a non-phylo (or multiPhylo) object is used in the phy",{
  expect_error(as.treedata.table(tree = anolis$dat, data = as.anolis$dat),
               "Please use a class 'phylo' or 'multiPhylo' tree \n", fixed=T)
})



test_that("Error is a non-data.frame is used in as.treedata.table", {
  expect_error(as.treedata.table(
    tree = anolis$phy,
    data = as.matrix(anolis$dat)
  ),
  "Your data MUST be of class data.frame",
  fixed = T
  )
})


test_that("Normal as.treedata.table", {
  expect_is(
    as.treedata.table(tree = anolis$phy, data = anolis$dat),
    "treedata.table"
  )
})


test_that("Normal as.treedata.table but data without column names", {
  data <- anolis$dat
  colnames(data) <- NULL
  expect_is(
    as.treedata.table(tree = anolis$phy, data = data),
    "treedata.table"
  )
})


test_that("Normal as.treedata.table but testing the no tips dropped message", {
  expect_message(as.treedata.table(tree = anolis$phy, data = anolis$dat),
    "No tips were dropped from the original tree/dataset",
    fixed = T
  )
})


test_that("Normal as.treedata.table with data.frame without row.names
          but testing the no tips dropped message", {
  data <- anolis$dat
  row.names(data) <- NULL
  expect_message(as.treedata.table(tree = anolis$phy, data = data),
    "No tips were dropped from the original tree/dataset",
    fixed = T
  )
})

test_that("Normal as.treedata.table but testing if the tips dropped message is
          shown for trees dropped from tree", {
  anolis1 <- anolis$phy
  anolis1$tip.label[1] <- "NAA"

  expect_message(as.treedata.table(tree = anolis1, data = anolis$dat),
    " tip(s) dropped from the original tree",
    fixed = T
  )
})


test_that("Normal as.treedata.table but testing if the tips dropped message is
          shown for trees dropped from data",{
  anolis1 <- anolis$phy
  anolis1$tip.label[1] <- "NAA"

  expect_message(as.treedata.table(tree = anolis1, data = anolis$dat),
                 "dropped from the original dataset", fixed=T)

})

test_that("Message when dropping taxa droptreedata.table", {
  expect_message(droptreedata.table(tdObject = td, taxa = c(
    "chamaeleonides",
    "eugenegrahami"
  )),
  "2 taxa were dropped from the treedata.table object",
  fixed = T
  )
})


test_that("Error when a non-treedata.table object is
          used in droptreedata.table", {
  expect_error(droptreedata.table(tdObject = td$phy, taxa = c(
    "chamaeleonides",
    "eugenegrahami"
  )),
  "Please use a class 'treedata.table' object \n",
  fixed = T
  )
})



test_that("Expect a phylo object when dropping taxa from phylo in
          droptreedata.table", {
  expect_is(
    droptreedata.table(tdObject = td, taxa = c(
      "chamaeleonides",
      "eugenegrahami"
    ))$phy,
    "phylo"
  )
})


test_that("Expect a multiphylo object when dropping taxa from phylo in
          droptreedata.table", {
  treesFM <- list(anolis$phy, anolis$phy)
  class(treesFM) <- "multiPhylo"
  td <- as.treedata.table(treesFM, anolis$dat)
  expect_is(
    droptreedata.table(tdObject = td, taxa = c(
      "chamaeleonides",
      "eugenegrahami"
    ))$phy,
    "multiPhylo"
  )
})

test_that("Error when a non-character vector is used in droptreedata.table", {
  expect_error(droptreedata.table(tdObject = td, taxa = 1),
    "Please use a class 'character' object for taxa \n",
    fixed = T
  )
})



test_that("Error when a non-treedata.table object in extractVector", {
  expect_error(extractVector(td$phy, "SVL", "ecomorph"),
    "Please use a class 'treedata.table' object \n",
    fixed = T
  )
})


test_that("Expect list when using extractVector with multiple arguments", {
  expect_is(extractVector(td, "SVL", "ecomorph"), "list")
})


test_that("Expect list when using extractVector with multiple arguments
          (for SVL)", {
  expect_is(extractVector(td, "SVL"), "numeric")
})


test_that("Test if extractVector works with multiPhylo", {
  treesFM <- list(anolis$phy, anolis$phy)
  class(treesFM) <- "multiPhylo"
  td <- as.treedata.table(treesFM, anolis$dat)
  expect_is(extractVector(td, "SVL"), "numeric")
})


test_that("[.treedata.table for multiphylo produces a data.table as output", {
  treesFM <- list(anolis$phy, anolis$phy)
  class(treesFM) <- "multiPhylo"
  td <- as.treedata.table(treesFM, anolis$dat)
  expect_is(td[, SVL]$dat, "data.table")
})


test_that("[.treedata.table for multiphylo produces a multiphylo", {
  treesFM <- list(anolis$phy, anolis$phy)
  class(treesFM) <- "multiPhylo"
  td <- as.treedata.table(treesFM, anolis$dat)
  expect_is(td[, SVL]$phy, "multiPhylo")
})


test_that("[.treedata.table for multiphylo produces a vector as output", {
  treesFM <- list(anolis$phy, anolis$phy)
  class(treesFM) <- "multiPhylo"
  td <- as.treedata.table(treesFM, anolis$dat)
  expect_is(td[["SVL"]], "numeric")
})


test_that("Error when a non-treedata.table object in tdt", {
  expect_error(tdt(td$phy, geiger::fitContinuous(phy, extractVector(td, "SVL"),
    model = "BM", ncores = 1
  )),
  "Please use a class 'treedata.table' object \n",
  fixed = T
  )
})




test_that("Single list when using tdt on phylo", {
  expect_is(
    tdt(td, geiger::fitContinuous(phy, extractVector(td, "SVL"),
      model = "BM",
      ncores = 1
    )),
    "list"
  )
})

test_that("Expect list with lenght >1 when using tdt on multiPhylo", {
  treesFM <- list(anolis$phy, anolis$phy)
  class(treesFM) <- "multiPhylo"
  td <- as.treedata.table(treesFM, anolis$dat)
  out <- tdt(td, geiger::fitContinuous(phy, extractVector(td, "SVL"),
    model = "BM", ncores = 1
  ))
  expect_equal(length(td), length(out))
})


test_that("Message when using tdt on multiPhylo", {
  treesFM <- list(anolis$phy, anolis$phy)
  class(treesFM) <- "multiPhylo"
  td <- as.treedata.table(treesFM, anolis$dat)
  expect_message(tdt(td, geiger::fitContinuous(phy, extractVector(td, "SVL"),
    model = "BM", ncores = 1
  )),
  "Multiphylo object detected. Expect a list of function outputs",
  fixed = T
  )
})

test_that("detectCharacterType", {
  data(anolis)
  expect_is(detectCharacterType(anolis$dat[, 1]), "character")
})

test_that("detectCharacterType warning", {
  dat <- c(rep("1", 20, ), rep("2", 20))
  expect_warning(detectCharacterType(dat),
    "Guessing this is a discrete character based on repeated values",
    fixed = T
  )
})

test_that("filterMatrix testing", {
  expect_equal(ncol(filterMatrix(anolis$dat, "discrete")), 3)
})


test_that("hasNames anolis$dat rows with rownames", {
  expect_true(hasNames(anolis$dat, "row"))
})


test_that("hasNames anolis$dat rows without rownames", {
  df <- as.matrix(data.frame(a = c(2, 3, 5), b = c("A", "f", "E")))
  expect_false(hasNames(df, "row"))
})

test_that("hasNames anolis$dat cols with colnames", {
  expect_true(hasNames(anolis$dat, "col"))
})


test_that("hasNames anolis$dat cols without colnames", {
  df <- as.matrix(data.frame(a = c(2, 3, 5), b = c("A", "f", "E")))
  colnames(df) <- NULL
  expect_false(hasNames(df, "col"))
})


test_that("hasNames anolis$dat rows and cols with names", {
  expect_true(hasNames(anolis$dat, "rowcol"))
})


test_that("hasNames anolis$dat rows and cols with names", {
  df <- as.matrix(data.frame(a = c(2, 3, 5), b = c("A", "f", "E")))
  colnames(df) <- NULL
  expect_false(hasNames(df, "rowcol"))
})


test_that("forcenames anolis$dat rows and cols with names", {
  expect_equal(forceNames(anolis$dat, "row"), anolis$dat)
})

test_that("forcenames without rownames", {
  df <- as.matrix(data.frame(a = c(2, 3, 5), b = c("A", "f", "E")))
  expect_is(row.names(forceNames(df, "row")), "character")
})


test_that("Print() prints the phylo", {
  expect_output(print(td), "$phy \n", fixed = T)
})

test_that("Print() prints the data", {
  expect_output(print(td), "$dat \n", fixed = T)
})


test_that("Summary() using a treedata.table object", {
  expect_message(summary(td), "A treedata.table object", fixed = T)
})

test_that("Summary() detecting continuous characters", {
  expect_message(summary(td), "Continuous traits: ", fixed = T)
})

test_that("Summary() detecting discrete characters", {
  expect_message(summary(td), "Discrete traits: ", fixed = T)
})


test_that("Summary() detecting discrete characters", {
  expect_message(summary(td), "The following traits have missing values:",
    fixed = T
  )
})



test_that("Summary() detecting taxa dropped", {
  expect_message(summary(td), "Taxa dropped from the tree:", fixed = T)
})
