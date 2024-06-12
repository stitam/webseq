test_that("ena2ncbi() works with a single query", {
  A <- ena2ncbi("ERS15941089")
  expect_equal(dim(A), c(1,2))
  expect_equal(names(A), c("ena", "ncbi"))
  expect_equal(A$ena, "ERS15941089")
  expect_equal(A$ncbi, "SAMEA113946840")
})

test_that("ena2ncbi() works with multiple queries", {
  B <- ena2ncbi(c("ERS15941089", "ERS15939592"))
  expect_equal(dim(B), c(2,2))
  expect_equal(B$ena, c("ERS15941089", "ERS15939592"))
  expect_equal(B$ncbi, c("SAMEA113946840", "SAMEA113945342"))
})

test_that("ena2ncbi() returns results in the same order as the query", {
  C <- ena2ncbi(c("ERS15939592", "ERS15941089"))
  expect_equal(C$ena, c("ERS15939592", "ERS15941089"))
  expect_equal(C$ncbi, c("SAMEA113945342", "SAMEA113946840"))
})

test_that("ena2ncbi() returns an empty data frame when given an NCBI ID", {
  D <- suppressWarnings(ena2ncbi("SAMEA111452506"))
  D_msg <- capture_warnings(ena2ncbi("SAMEA111452506"))
  expect_equal(dim(D), c(0,2))
  expect_equal(class(D$ena), "character")
  expect_equal(class(D$ncbi), "character")
  expect_equal(D_msg, "The following accessions were not found: SAMEA111452506")
})

test_that("ena2ncbi() removes invalid queries and returns the rest", {
  E <- suppressWarnings(ena2ncbi(c("ERS15939592", "balloon", "ERS15941089")))
  expect_equal(dim(E), c(2,2))
})


