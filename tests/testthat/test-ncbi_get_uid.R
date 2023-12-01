data(examples)

test_that("ncbi_get_uid() works for a single query",{
  
  res <- ncbi_get_uid(examples$assembly[1], db = "assembly")
  
  expect_type(res, "list")
  expect_s3_class(res$uids, c("tbl_df", "tbl", "data.frame"))
  expect_type(res$web_history, "character")
  expect_true(res$uids$uid == 5197591)
  expect_false(is.null(res$web_history))
  
})

test_that("ncbi_get_uid() works for a single query without web history",{
  
  res <- ncbi_get_uid(
    examples$assembly[1], db = "assembly", use_history = FALSE)
  
  expect_type(res, "list")
  expect_s3_class(res$uids, c("tbl_df", "tbl", "data.frame"))
  expect_true(res$uids$uid == 5197591)
  expect_true(is.null(res$web_history))
  
})

test_that("ncbi_get_uid() handles NA", {
  expect_error(ncbi_get_uid(term = NA, db = "assembly"))
  
  res <- ncbi_get_uid(
    term = c(NA, examples$assembly[1], NA, examples$assembly[2]),
    db = "assembly",
    verbose = TRUE
  )
  
  res_messages <- testthat::capture_messages(ncbi_get_uid(
    term = c(NA, examples$assembly[1], NA, examples$assembly[2]),
    db = "assembly",
    verbose = TRUE
  ))
  
  expect_type(res, "list")
  expect_s3_class(res$uids, c("tbl_df", "tbl", "data.frame"))
  expect_type(res$web_history, "character")
  expect_equal(nrow(res$uids), 2)
  
  expect_true(res_messages[1] == "Removing NA-s from search terms.\n")
  expect_true(res_messages[2] == "Querying UIDs for batch 1. ")
  expect_true(res_messages[3] == "OK.\n")
})

test_that("ncbi_get_uid() handles invalid terms", {
  res <- ncbi_get_uid(term = "noname", db = "assembly", verbose = TRUE)
  res_messages <- capture_message(
    res <- ncbi_get_uid(term = "noname", db = "assembly", verbose = TRUE)
  )
  
  expect_type(res, "list")
  expect_s3_class(res$uids, c("tbl_df", "tbl", "data.frame"))
  expect_true(is.na(res$uids$uid))
  expect_true(is.null(res$web_history))
})

test_that("ncbi_get_uid works with a complex term", {
  res <- ncbi_get_uid("Autographiviridae OR Podoviridae", db = "assembly")

  expect_true(nrow(res$uids) > 3000)
})
