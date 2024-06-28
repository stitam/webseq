data(examples)

test_that("ncbi_get_summary() works with a single query", {
  A <- "GCF_000002435.2"
  uid <- ncbi_get_uid(A, "assembly")
  res <- ncbi_get_summary(uid)
  
  expect_type(res, "list")
  expect_equal(length(res), 1)
})

test_that("ncbi_get_summary() works with multiple queries", {
  assemblies <- c("GCF_000002435.2", "GCF_000299415.1")
  uids <- ncbi_get_uid(assemblies, db = "assembly")
  res <- ncbi_get_summary(uids)
  
  expect_type(res, "list")
  expect_equal(length(res), 2)
})

test_that("ncbi_get_summary() works with UIDs", {
  assemblies <- c("GCF_000002435.2", "GCF_000299415.1")
  uids <- ncbi_get_uid(assemblies, db = "assembly")
  res <- ncbi_get_summary(uids$uid, db = "assembly")
  
  expect_type(res, "list")
  expect_equal(length(res), 2)
})
