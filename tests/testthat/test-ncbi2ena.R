test_that("ncbi2ena() works with a single query", {
  A <- ncbi2ena("SAMEA113946840", type = "biosample")
  expect_equal(dim(A), c(1,2))
  expect_equal(names(A), c("ncbi", "ena"))
  expect_equal(A$ncbi, "SAMEA113946840")
  expect_equal(A$ena, "ERS15941089")
})

test_that("ncbi2ena() works with multiple queries", {
  B <- ncbi2ena(c("SAMEA113946840", "SAMEA113945342"), type = "biosample")
  expect_equal(dim(B), c(2,2))
  expect_equal(B$ncbi, c("SAMEA113946840", "SAMEA113945342"))
  expect_equal(B$ena, c("ERS15941089", "ERS15939592"))
  
})

test_that("ncbi2ena() returns results in the same order as the query", {
  C <- ncbi2ena(c("SAMEA113945342", "SAMEA113946840"), type = "biosample")
  expect_equal(C$ncbi, c("SAMEA113945342", "SAMEA113946840"))
  expect_equal(C$ena, c("ERS15939592", "ERS15941089"))
})

test_that("ncbi2ena() returns an empty data frame when given an ENA ID", {
  D <- suppressWarnings(ncbi2ena("ERS15941089", type = "biosample"))
  D_msg <- capture_warnings(ncbi2ena("ERS15941089", type = "biosample"))
  expect_equal(dim(D), c(0,2))
  expect_equal(class(D$ncbi), "character")
  expect_equal(class(D$ena), "character")
  expect_equal(D_msg, "The following accessions were not found: ERS15941089")
})

test_that("ncbi2ena() removes invalid queries and returns the rest", {
  E <- suppressWarnings(ncbi2ena(
      c("SAMEA113945342", "balloon", "SAMEA113946840"),
      type = "biosample"
  ))
  expect_equal(dim(E), c(2,2))
})
